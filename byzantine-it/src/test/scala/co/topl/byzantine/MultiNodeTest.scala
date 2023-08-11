package co.topl.byzantine

import cats.effect.IO
import cats.effect.implicits._
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.blockchain.{PrivateTestnet, StakerInitializers}
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.{Datum, Event, TransactionOutputAddress}
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.byzantine.util._
import co.topl.codecs.bytes.tetra.instances.persistableKesProductSecretKey
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.crypto.generation.mnemonic.Entropy
import co.topl.crypto.models.SecretKeyKesProduct
import co.topl.crypto.signing.{Ed25519, Ed25519VRF, KesProduct}
import com.spotify.docker.client.DockerClient
import org.typelevel.log4cats.Logger
import co.topl.interpreters.NodeRpcOps._
import co.topl.node.StakingInit
import com.google.protobuf.ByteString
import fs2.Chunk
import fs2.io.file.Files
import co.topl.numerics.implicits._

import java.security.SecureRandom
import java.time.Instant
import scala.concurrent.duration._

class MultiNodeTest extends IntegrationSuite {

  override def munitTimeout: Duration = 15.minutes

  test("Multiple nodes launch and maintain consensus for three epochs") {
    val epochSlotLength: Long = 6 * 50 // See co.topl.node.ApplicationConfig.Bifrost.Protocol
    val bigBang = Instant.now().plusSeconds(30)
    val config0 = TestNodeConfig(bigBang, 3, 0, Nil)
    val config1 = TestNodeConfig(bigBang, 3, 1, List("MultiNodeTest-node0"))
    val config2 = TestNodeConfig(bigBang, 3, 2, List("MultiNodeTest-node1"))
    val config3 = TestNodeConfig(bigBang, 3, -1, List("MultiNodeTest-node2"))
    val resource =
      for {
        (dockerSupport, _dockerClient) <- DockerSupport.make[F]()
        implicit0(dockerClient: DockerClient) = _dockerClient
        node0 <- dockerSupport.createNode("MultiNodeTest-node0", "MultiNodeTest", config0)
        node1 <- dockerSupport.createNode("MultiNodeTest-node1", "MultiNodeTest", config1)
        node2 <- dockerSupport.createNode("MultiNodeTest-node2", "MultiNodeTest", config2)
        nodes = List(node0, node1, node2)
        _ <- nodes.parTraverse(_.startContainer[F]).toResource
        _ <- nodes
          .parTraverse(node => node.rpcClient[F](node.config.rpcPort, tls = false).use(_.waitForRpcStartUp))
          .toResource
        _ <- Logger[F].info("Waiting for nodes to reach target epoch.  This may take several minutes.").toResource
        thirdEpochHeads <- nodes
          .parTraverse(node =>
            node
              .rpcClient[F](node.config.rpcPort, tls = false)
              .use(_.adoptedHeaders.takeWhile(_.slot < (epochSlotLength * 3)).timeout(9.minutes).compile.lastOrError)
          )
          .toResource
        _     <- Logger[F].info("Nodes have reached target epoch").toResource
        node3 <- dockerSupport.createNode("MultiNodeTest-node3", "MultiNodeTest", config3)
        heights = thirdEpochHeads.map(_.height)
        // All nodes should be at _roughly_ equal height
        _ <- IO(heights.max - heights.min <= 5).assert.toResource
        // All nodes should have a shared common ancestor near the tip of the chain
        _ <- nodes
          .parTraverse(node =>
            node
              .rpcClient[F](node.config.rpcPort, tls = false)
              .use(
                _.blockIdAtHeight(heights.min - 5)
              )
          )
          .map(_.toSet.size)
          .assertEquals(1)
          .toResource
      } yield ()

    resource.use_

  }

  private def registerStaker(inputTransaction: IoTransaction, inputIndex: Int)(node: BifrostDockerNode) =
    Files
      .forAsync[F]
      .tempDirectory
      .use(localTmpDir =>
        for {
          kesKey <- Sync[F]
            .delay(new KesProduct().createKeyPair(SecureRandom.getInstanceStrong.generateSeed(32), (9, 9), 0))
          operatorKey <- Sync[F].delay(new Ed25519().deriveKeyPairFromEntropy(Entropy.generate(), None))
          vrfKey      <- Sync[F].delay(Ed25519VRF.precomputed().generateRandom)
          writeFile = (name: String, data: Array[Byte]) =>
            fs2.Stream
              .chunk(Chunk.array(data))
              .through(
                Files
                  .forAsync[F]
                  .writeAll(
                    localTmpDir / name
                  )
              )
              .compile
              .drain
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
          input = SpentTransactionOutput(
            TransactionOutputAddress(0, 0, inputIndex, inputTransaction.id),
            ???,
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
          transaction = IoTransaction(datum =
            Datum.IoTransaction(
              Event.IoTransaction.defaultInstance.withSchedule(
                Schedule(0L, Long.MaxValue, System.currentTimeMillis())
              )
            )
          )
            .withInputs(List(input))
            .withOutputs(registrationOutputs :+ changeOutput)
        } yield ()
      )
}
