package co.topl.byzantine

import cats.data.OptionT
import cats.effect.IO
import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.algebras.NodeRpc
import co.topl.blockchain.{PrivateTestnet, StakerInitializers}
import co.topl.brambl.common.ContainsSignable.ContainsSignableTOps
import co.topl.brambl.common.ContainsSignable.instances.ioTransactionSignable
import co.topl.brambl.models.box.{Attestation, Value}
import co.topl.brambl.models.{Datum, Event, TransactionOutputAddress}
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.byzantine.util._
import co.topl.codecs.bytes.tetra.instances.persistableKesProductSecretKey
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.consensus.models.StakingAddress
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import com.spotify.docker.client.DockerClient
import org.typelevel.log4cats.Logger
import co.topl.interpreters.NodeRpcOps._
import co.topl.node.StakingInit
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.file.{Files, Path, PosixPermission, PosixPermissions}
import co.topl.numerics.implicits._
import co.topl.quivr.api.Prover

import java.security.SecureRandom
import java.time.Instant
import scala.concurrent.duration._

class MultiNodeTest extends IntegrationSuite {
  import MultiNodeTest._

  override def munitTimeout: Duration = 15.minutes
  // This many nodes will be launched for this test.  All but one of the nodes will be launched immediately as
  // genesis stakers.  The final node will be launched later in the test.
  // When running on GitHub Actions, only 3 nodes can run on one machine.
  private val totalNodeCount = 3
  require(totalNodeCount >= 3)

  test("Multiple nodes launch and maintain consensus for three epochs") {
    val epochSlotLength: Long = 6 * 50 // See co.topl.node.ApplicationConfig.Bifrost.Protocol
    val bigBang = Instant.now().plusSeconds(30)
    val resource =
      for {
        (dockerSupport, _dockerClient) <- DockerSupport.make[F]()
        implicit0(dockerClient: DockerClient) = _dockerClient
        initialNodes <- List
          .tabulate(totalNodeCount - 1)(index =>
            dockerSupport.createNode(
              s"MultiNodeTest-node$index",
              "MultiNodeTest",
              TestNodeConfig(
                bigBang,
                totalNodeCount - 1,
                index,
                if (index == 0) Nil else List(s"MultiNodeTest-node${index - 1}")
              )
            )
          )
          .sequence
        node0 = initialNodes(0)
        node1 = initialNodes(1)
        _ <- initialNodes.parTraverse(_.startContainer[F]).toResource
        _ <- initialNodes
          .parTraverse(node => node.rpcClient[F](node.config.rpcPort).use(_.waitForRpcStartUp))
          .toResource
        genesisTransaction <- node0
          .rpcClient[F](node0.config.rpcPort)
          .use(client =>
            OptionT(client.blockIdAtHeight(1))
              .flatMapF(client.fetchBlockBody)
              .map(_.transactionIds.head)
              .flatMapF(client.fetchTransaction)
              .getOrRaise(new IllegalStateException)
          )
          .toResource
        // Node 3 is a delayed staker.  It will register in epoch 0, but the container won't launch until epoch 2.
        tmpHostStakingDirectory <- Files.forAsync[F].tempDirectory
        _ <- Files.forAsync[F].setPosixPermissions(tmpHostStakingDirectory, PosixOtherWritePermissions).toResource
        delayedNodeConfig = TestNodeConfig(
          bigBang,
          totalNodeCount - 1,
          -1,
          List("MultiNodeTest-node1"),
          stakingBindSourceDir = tmpHostStakingDirectory.toString.some
        )
        delayedNodeName = s"MultiNodeTest-node${totalNodeCount - 1}"
        delayedNode <- dockerSupport.createNode(
          delayedNodeName,
          "MultiNodeTest",
          delayedNodeConfig
        )
        allNodes = initialNodes :+ delayedNode
        _ <- Logger[F].info(s"Registering $delayedNodeName").toResource
        delayedNodeStakingAddress <- node1
          .rpcClient[F](node1.config.rpcPort)
          // Take stake from node0 and transfer it to the delayed node
          .evalMap(registerStaker(genesisTransaction, 0)(_, tmpHostStakingDirectory))
        _ <- Logger[F].info("Waiting for nodes to reach target epoch.  This may take several minutes.").toResource
        thirdEpochHeads <- initialNodes
          .parTraverse(node =>
            node
              .rpcClient[F](node.config.rpcPort)
              .use(_.adoptedHeaders.takeWhile(_.slot < (epochSlotLength * 3)).timeout(9.minutes).compile.lastOrError)
          )
          .toResource
        _ <- Logger[F].info("Nodes have reached target epoch").toResource
        // The delayed node's registration should now be active, so the delayed node can launch and start producing blocks
        _ <- Logger[F].info(s"Starting $delayedNodeName").toResource
        _ <- delayedNode.startContainer[F].toResource
        // The delayed node's blocks should be valid on other nodes (like node0), so search node0 for adoptions of a block produced
        // by the delayed node's staking address
        _ <- node0
          .rpcClient[F](node0.config.rpcPort)
          .use(_.adoptedHeaders.find(_.address == delayedNodeStakingAddress).timeout(2.minutes).compile.lastOrError)
          .toResource
        heights = thirdEpochHeads.map(_.height)
        // All nodes should be at _roughly_ equal height
        _ <- IO(heights.max - heights.min <= 5).assert.toResource
        // All nodes should have a shared common ancestor near the tip of the chain
        _ <- allNodes
          .parTraverse(node =>
            node
              .rpcClient[F](node.config.rpcPort)
              .use(_.blockIdAtHeight(heights.min - 5))
          )
          .map(_.toSet.size)
          .assertEquals(1)
          .toResource
      } yield ()

    resource.use_

  }

  /**
   * Generates a new staker by taking half of the stake from the given input transaction.  The keys are generated locally
   * on the host machine before being copied into the target container.  The resulting registration transaction is also
   * broadcasted using the given RPC client.
   * @param inputTransaction A transaction containing funds to split
   * @param inputIndex The UTxO index at which the funds currently exist
   * @param rpcClient the RPC client to receive the broadcasted transaction
   * @return the new staker's StakingAddress
   */
  private def registerStaker(inputTransaction: IoTransaction, inputIndex: Int)(
    rpcClient:   NodeRpc[F, fs2.Stream[F, *]],
    localTmpDir: Path
  ): F[StakingAddress] =
    for {
      _ <- Logger[F].info("Generating new staker keys")
      kesKey <- Sync[F]
        .delay(new KesProduct().createKeyPair(SecureRandom.getInstanceStrong.generateSeed(32), (9, 9), 0))
      operatorKey <- Sync[F].delay(new Ed25519().deriveKeyPairFromEntropy(Entropy.generate(), None))
      vrfKey      <- Sync[F].delay(Ed25519VRF.precomputed().generateRandom)
      writeFile = (name: String, data: Array[Byte]) =>
        fs2.Stream.chunk(Chunk.array(data)).through(Files.forAsync[F].writeAll(localTmpDir / name)).compile.drain
      _ <- Logger[F].info("Saving new staker keys to temp directory")
      _ <- Files[F].createDirectory(localTmpDir / StakingInit.KesDirectoryName)
      _ <- Files.forAsync[F].setPosixPermissions(localTmpDir / StakingInit.KesDirectoryName, PosixOtherWritePermissions)
      _ <- Files[F].createFile(localTmpDir / StakingInit.KesDirectoryName / "0", Some(PosixOtherWritePermissions))
      _ <- Files
        .forAsync[F]
        .setPosixPermissions(localTmpDir / StakingInit.KesDirectoryName / "0", PosixOtherWritePermissions)
      _ <- writeFile(
        StakingInit.KesDirectoryName + "/0",
        Persistable[SecretKeyKesProduct].persistedBytes(kesKey._1).toByteArray
      )
      _ <- writeFile(StakingInit.OperatorKeyName, operatorKey.signingKey.bytes)
      _ <- writeFile(StakingInit.VrfKeyName, vrfKey._1)
      stakerInitializer =
        StakerInitializers.Operator(
          ByteString.copyFrom(operatorKey.signingKey.bytes),
          PrivateTestnet.HeightLockOneSpendingAddress,
          ByteString.copyFrom(vrfKey._1),
          kesKey._1
        )
      spendableOutput = inputTransaction.outputs(inputIndex)
      unprovenPredicateAttestation = Attestation.Predicate(PrivateTestnet.HeightLockOneLock.getPredicate, Nil)
      unprovenInput = SpentTransactionOutput(
        TransactionOutputAddress(0, 0, inputIndex, inputTransaction.id),
        Attestation.defaultInstance.withPredicate(unprovenPredicateAttestation),
        spendableOutput.value
      )
      spendableTopl = spendableOutput.value.value.topl.get
      spendableQuantity = spendableTopl.quantity: BigInt
      registrationOutputs = stakerInitializer.registrationOutputs(spendableQuantity / 2)
      changeOutput = UnspentTransactionOutput(
        PrivateTestnet.HeightLockOneSpendingAddress,
        Value.defaultInstance.withTopl(
          Value.TOPL(
            spendableQuantity - (spendableQuantity / 2),
            spendableTopl.registration
          )
        )
      )
      unprovenTransaction = IoTransaction(datum =
        Datum.IoTransaction(
          Event.IoTransaction.defaultInstance.withSchedule(
            Schedule(0L, Long.MaxValue, System.currentTimeMillis())
          )
        )
      )
        .withInputs(List(unprovenInput))
        .withOutputs(registrationOutputs :+ changeOutput)

      proof <- Prover.heightProver[F].prove((), unprovenTransaction.signable)
      provenPredicateAttestation = unprovenPredicateAttestation.copy(responses = List(proof))
      transaction = unprovenTransaction.copy(
        inputs = unprovenTransaction.inputs.map(
          _.copy(attestation = Attestation(Attestation.Value.Predicate(provenPredicateAttestation)))
        )
      )
      _ <- Logger[F].info("Broadcasting registration transaction")
      _ <- rpcClient.broadcastTransaction(transaction)
      _ <- writeFile(StakingInit.RegistrationTxName, transaction.toByteArray)
    } yield stakerInitializer.stakingAddress
}

object MultiNodeTest {

  /**
   * The KES key needs to be modifiable by the node's container user
   */
  val PosixOtherWritePermissions: PosixPermissions =
    PosixPermissions(
      PosixPermission.OwnerRead,
      PosixPermission.OwnerWrite,
      PosixPermission.OwnerExecute,
      PosixPermission.GroupRead,
      PosixPermission.GroupWrite,
      PosixPermission.GroupExecute,
      PosixPermission.OthersRead,
      PosixPermission.OthersWrite,
      PosixPermission.OthersExecute
    )
}
