package co.topl.node

import cats.effect._
import cats.effect.implicits._
import cats.effect.kernel.{Async, Resource}
import cats.effect.std.SecureRandom
import cats.implicits._
import co.topl.algebras.{NodeRpc, SynchronizationTraversalSteps}
import co.topl.blockchain.{BigBang, PrivateTestnet, StakerInitializers, StakingInit}
import co.topl.brambl.models._
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction.{IoTransaction, UnspentTransactionOutput}
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances.blockHeaderAsBlockHeaderOps
import co.topl.config.ApplicationConfig
import co.topl.consensus.models.{BlockId, ProtocolVersion}
import co.topl.genus.services._
import co.topl.grpc.makeChannel
import co.topl.interpreters.NodeRpcOps.clientAsNodeRpcApi
import co.topl.models.utility._
import co.topl.node.models.{BlockBody, FullBlock}
import co.topl.node.services.NetworkControlRpcFs2Grpc
import co.topl.transactiongenerator.interpreters.GenusWalletInitializer
import co.topl.transactiongenerator.models._
import co.topl.typeclasses.implicits._
import com.comcast.ip4s.Port
import fs2.io.file.{Files, Path}
import fs2.{io => _, _}
import io.grpc.Metadata
import munit.CatsEffectAssertions.MUnitCatsAssertionsForIOBooleanOps
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.server._
import org.http4s.server.Router
import quivr.models.Int128
import co.topl.models.p2p.HostId
import co.topl.node.services._

import java.nio.charset.StandardCharsets
import scala.concurrent.duration._

object Util {
  type F[A] = IO[A]

  type RpcClient = NodeRpc[F, Stream[F, *]]

  def createTestnetConfig: F[TestnetConfig] =
    for {
      random <- SecureRandom.javaSecuritySecureRandom[F]
      protocol = PrivateTestnet.DefaultProtocol.copy(slotDuration = 500.milli)
      createStaker = random
        .nextBytes(32)
        .map(seed =>
          StakerInitializers
            .Operator(seed, (protocol.kesKeyHours, protocol.kesKeyHours), PrivateTestnet.HeightLockOneSpendingAddress)
        )
      staker0 <- createStaker
      staker1 <- createStaker
      unstakedTopl = UnspentTransactionOutput(
        PrivateTestnet.HeightLockOneSpendingAddress,
        Value.defaultInstance.withTopl(Value.TOPL(1_000_000L))
      )
      lvl = UnspentTransactionOutput(
        PrivateTestnet.HeightLockOneSpendingAddress,
        Value.defaultInstance.withLvl(Value.LVL(1_000_000L))
      )
      timestamp <- Async[F].realTime.map(_.plus(20.seconds).toMillis)
    } yield TestnetConfig(
      timestamp,
      List(staker0 -> 500_000L, staker1 -> 500_000L),
      List(unstakedTopl),
      List(lvl),
      protocol
    )

  /**
   * Launches an HTTP server which mimics the behavior of serving a genesis block from GitHub
   * @param genesis the block to serve
   * @return the localhost port number upon which the HTTP server is bound.  The port is randomly created based
   *         on host availability.
   */
  def serveGenesisBlock(genesis: FullBlock): Resource[F, Int] =
    for {
      genesisId <- Sync[F].delay(genesis.header.id).toResource
      genesisIdStr = genesisId.show
      files = Map(
        s"$genesisIdStr.header.pbuf" -> genesis.header.toByteArray,
        s"$genesisIdStr.body.pbuf" -> BlockBody(
          genesis.fullBody.transactions.map(_.id),
          genesis.fullBody.rewardTransaction.map(_.id)
        ).toByteArray
      ) ++ genesis.fullBody.transactions.map(tx => s"${tx.id.show}.transaction.pbuf" -> tx.toByteArray)
      routes =
        HttpRoutes.of[F] { case GET -> Root / `genesisIdStr` / fileName =>
          files.get(fileName).fold(NotFound())(Ok(_))
        }
      server <- EmberServerBuilder
        .default[F]
        .withPort(Port.fromInt(0).get)
        .withHttpApp(Router("/" -> routes).orNotFound)
        .build
      port = server.address.getPort
    } yield port

  def saveStaker(dir: Path)(staker: StakerInitializers.Operator, quantity: Int128): IO[Unit] = {
    def saveFile(name: String, data: Array[Byte]) =
      Stream.chunk(Chunk.array(data)).through(Files.forAsync[F].writeAll(dir / name)).compile.drain

    saveFile(StakingInit.OperatorKeyName, staker.operatorSK.toByteArray) *>
    saveFile(StakingInit.VrfKeyName, staker.vrfSK.toByteArray) *>
    Files.forAsync[F].createDirectories(dir / StakingInit.KesDirectoryName) *>
    saveFile(
      s"${StakingInit.KesDirectoryName}/0",
      co.topl.codecs.bytes.tetra.instances.persistableKesProductSecretKey.persistedBytes(staker.kesSK).toByteArray
    ) *>
    saveFile(StakingInit.RegistrationTxName, staker.registrationTransaction(quantity).toByteArray)
  }

  /**
   * Serve a config file on a random port.  Returns the URL to access the file
   * @param config the contents to serve
   * @param name the file name
   * @return a URL to the file
   */
  def serveConfig(config: String, name: String): Resource[F, String] =
    EmberServerBuilder
      .default[F]
      .withPort(Port.fromInt(0).get)
      .withHttpApp(Router("/" -> HttpRoutes.of[F] { case GET -> Root / `name` => Ok(config) }).orNotFound)
      .build
      .map(server => s"http://localhost:${server.address.getPort}/$name")

  def saveLocalConfig(config: String, name: String): Resource[F, Path] =
    for {
      file <- Files[F].tempFile(None, name, ".yaml", None)
      _ <- Stream
        .iterable(config.getBytes(StandardCharsets.UTF_8))
        .through(Files[F].writeAll(file))
        .compile
        .drain
        .toResource
    } yield file

  def fetchUntilHeight(rpcClient: RpcClient, height: Long): F[Unit] =
    Stream
      .force(rpcClient.synchronizationTraversal())
      .collect { case SynchronizationTraversalSteps.Applied(id) => id }
      .evalMap(rpcClient.fetchBlockHeader)
      .map(_.get)
      .takeWhile(_.height <= height)
      .compile
      .drain

  def fetchUntilTx(rpcClient: RpcClient, txId: TransactionId): F[Unit] =
    Stream
      .force(rpcClient.synchronizationTraversal())
      .collect { case SynchronizationTraversalSteps.Applied(id) => id }
      .evalMap(_ => txsInChain(rpcClient)(Set(txId)).map(r => !r))
      .takeWhile(identity)
      .compile
      .drain

  def launch(configLocation: String): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    for {
      app1                      <- Sync[F].delay(new AbstractNodeApp {}).toResource
      (args, config, appConfig) <- app1.initialize(Array("--config", configLocation)).toResource
      backgroundOutcomeF        <- app1.run(args, config, appConfig).background
    } yield backgroundOutcomeF

  def confirmTransactions(
    client: RpcClient
  )(ids: Set[TransactionId], confirmationDepth: Int = 3): F[Unit] = {
    def filterTransactions(targetBlock: BlockId)(ids: Set[TransactionId]): F[Set[TransactionId]] =
      client
        .fetchBlockBody(targetBlock)
        .map(ids -- _.get.allTransactionIds)
        .flatMap(ids =>
          if (ids.isEmpty) ids.pure[F]
          else
            client
              .fetchBlockHeader(targetBlock)
              .map(_.get)
              .flatMap(header =>
                if (header.height > 1) filterTransactions(header.parentHeaderId)(ids)
                else ids.pure[F]
              )
        )

    Stream
      .retry(
        client
          .blockIdAtDepth(confirmationDepth)
          .iterateUntil(_.nonEmpty)
          .map(_.get)
          .flatMap(filterTransactions(_)(ids))
          .map(_.isEmpty)
          .assert,
        1500.milli,
        identity,
        30
      )
      .compile
      .drain
  }

  def txsInChain(client: RpcClient)(ids: Set[TransactionId]): IO[Boolean] =
    client.history
      .flatMap(block => Stream.emits(block.fullBody.allTransactions))
      .map(_.id)
      .filter(ids.contains)
      .compile
      .count
      .map(_ == ids.size)

  def verifyConfirmed(client: RpcClient)(ids: Set[TransactionId]): IO[Unit] = txsInChain(client)(ids).assert

  def verifyNotConfirmed(client: RpcClient)(ids: Set[TransactionId]): IO[Unit] =
    client.history
      .flatMap(block => Stream.emits(block.fullBody.allTransactions))
      .map(_.id)
      .forall(!ids.contains(_))
      .compile
      .lastOrError
      .assert

  def awaitGenusReady(blockService: BlockServiceFs2Grpc[F, Metadata]): F[Unit] =
    blockService
      .getBlockByHeight(GetBlockByHeightRequest(ChainDistance(1)), new Metadata())
      .void
      .handleErrorWith(_ => Async[F].delayBy(awaitGenusReady(blockService), 1.seconds))

  def makeWallet[F[_]: Async](genusRpc: TransactionServiceFs2Grpc[F, Metadata]): Resource[F, Wallet] =
    GenusWalletInitializer.make[F](genusRpc).flatMap(w => w.initialize.toResource).flatMap {
      case w if w.spendableBoxes.isEmpty => makeWallet(genusRpc)
      case w                             => w.pure[F].toResource
    }
}

case class TestnetConfig(
  timestamp:     Long,
  stakers:       List[(StakerInitializers.Operator, Int128)],
  unstakedTopls: List[UnspentTransactionOutput],
  lvls:          List[UnspentTransactionOutput],
  protocol:      ApplicationConfig.Bifrost.Protocol
) {

  val protocolUtxo: UnspentTransactionOutput =
    UnspentTransactionOutput(PrivateTestnet.HeightLockOneSpendingAddress, BigBang.protocolToValue(protocol))

  val genesisTransactions: List[IoTransaction] =
    stakers.map { case (init, quantity) => init.registrationTransaction(quantity) } :+ IoTransaction.defaultInstance
      .withOutputs(unstakedTopls ++ lvls :+ protocolUtxo)

  val genesis: FullBlock =
    BigBang.fromConfig(
      BigBang.Config(
        timestamp,
        genesisTransactions,
        protocolVersion = ProtocolVersion(2, 0, 0)
      )
    )
}

trait NodeControlRpc[F[_]] {
  def getHostId(): F[HostId]
  def forgetPeer(hostId: HostId): F[Unit]
  def addPeer(ip:        String, port: Int, HostIdOpt: Option[HostId]): F[Unit]
}

object NetworkControlClient {

  def make[F[_]: Async](host: String, port: Int, tls: Boolean): Resource[F, NodeControlRpc[F]] =
    makeChannel(host, port, tls)
      .flatMap(NetworkControlRpcFs2Grpc.stubResource[F])
      .map(client =>
        new NodeControlRpc[F] {

          override def getHostId(): F[HostId] =
            client.getHostId(GetHostIdReq(), new Metadata()).map(resp => HostId(resp.id.id))

          override def forgetPeer(hostId: HostId): F[Unit] =
            client.forgetPeer(ForgetPeerReq(RpcHostId(hostId.id)), new Metadata()).void

          override def addPeer(ip: String, port: Int, HostIdOpt: Option[HostId]): F[Unit] =
            client.addPeer(AddPeerReq(HostIdOpt.map(id => RpcHostId(id.id)), ip, port), new Metadata()).void
        }
      )
}
