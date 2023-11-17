package co.topl.networking.fsnetwork

import cats.{Applicative, MonadThrow}
import cats.data.{Validated, ValidatedNec}
import cats.effect.kernel.Sync
import cats.effect.{Async, IO, Ref, Resource}
import cats.implicits._
import co.topl.algebras.testInterpreters.TestStore
import co.topl.brambl.generators.ModelGenerators.arbitraryIoTransaction
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.{Datum, TransactionId}
import co.topl.brambl.syntax.ioTransactionAsTransactionSyntaxOps
import co.topl.brambl.validation.algebras.TransactionSyntaxVerifier
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras._
import co.topl.consensus.models._
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.interpreters.SchedulerClock
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models.ModelGenerators.GenHelper
import co.topl.models.generators.consensus.ModelGenerators.arbitrarySlotData
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.ActorPeerHandlerBridgeAlgebraTest._
import co.topl.networking.fsnetwork.BlockDownloadError.BlockBodyOrTransactionError
import co.topl.networking.fsnetwork.TestHelper.BlockBodyOrTransactionErrorByName
import co.topl.networking.p2p.{ConnectedPeer, DisconnectedPeer, PeerConnectionChange, RemoteAddress}
import co.topl.node.models._
import co.topl.quivr.runtime.DynamicContext
import co.topl.typeclasses.implicits._
import fs2.Stream
import fs2.concurrent.Topic
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object ActorPeerHandlerBridgeAlgebraTest {
  type F[A] = IO[A]

  val genesisSlotData: SlotData = arbitrarySlotData.arbitrary.first.copy(height = 0)

  val chainSelectionAlgebra: ChainSelectionAlgebra[F, SlotData] =
    (x: SlotData, y: SlotData) => x.height.compare(y.height).pure[F]

  val networkProperties: NetworkProperties = NetworkProperties()

  val headerValidation: BlockHeaderValidationAlgebra[F] = new BlockHeaderValidationAlgebra[F] {

    override def validate(header: BlockHeader): F[Either[BlockHeaderValidationFailure, BlockHeader]] =
      Either.right[BlockHeaderValidationFailure, BlockHeader](header).pure[F]

    override def couldBeValidated(header: BlockHeader, currentHead: SlotData): F[Boolean] = true.pure[F]
  }

  val headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F] =
    (block: Block) => Either.right[BlockHeaderToBodyValidationFailure, Block](block).pure[F]

  val defaultTxSyntaxValidator: TransactionSyntaxVerifier[F] = (t: IoTransaction) => Either.right(t).pure[F]

  val bodySyntaxValidation: BodySyntaxValidationAlgebra[F] =
    (blockBody: BlockBody) => Validated.validNec[BodySyntaxError, BlockBody](blockBody).pure[F]

  val bodySemanticValidation: BodySemanticValidationAlgebra[F] = new BodySemanticValidationAlgebra[F] {

    def validate(context: BodyValidationContext)(blockBody: BlockBody): F[ValidatedNec[BodySemanticError, BlockBody]] =
      Validated.validNec[BodySemanticError, BlockBody](blockBody).pure[F]
  }

  val bodyAuthorizationValidation: BodyAuthorizationValidationAlgebra[F] = new BodyAuthorizationValidationAlgebra[F] {

    override def validate(context: IoTransaction => DynamicContext[F, HostId, Datum])(
      blockBody: BlockBody
    ): F[ValidatedNec[BodyAuthorizationError, BlockBody]] =
      Validated.validNec[BodyAuthorizationError, BlockBody](blockBody).pure[F]
  }

  def createSlotDataStore: F[TestStore[F, BlockId, SlotData]] =
    TestStore.make[F, BlockId, SlotData]

  def createHeaderStore: F[TestStore[F, BlockId, BlockHeader]] =
    TestStore.make[F, BlockId, BlockHeader]

  def createBodyStore: F[TestStore[F, BlockId, BlockBody]] =
    TestStore.make[F, BlockId, BlockBody]

  def createTransactionStore: F[TestStore[F, TransactionId, IoTransaction]] =
    TestStore.make[F, TransactionId, IoTransaction]

  def createRemotePeerStore: F[TestStore[F, Unit, Seq[RemotePeer]]] =
    TestStore.make[F, Unit, Seq[RemotePeer]]

  def createBlockIdTree: F[ParentChildTree[F, BlockId]] =
    ParentChildTree.FromRef.make[F, BlockId]

  def createEmptyLocalChain: LocalChainAlgebra[F] = new LocalChainAlgebra[F] {
    private var currentHead: SlotData = genesisSlotData

    override def isWorseThan(newHead: SlotData): F[Boolean] =
      (newHead.height > currentHead.height).pure[F]

    override def adopt(newHead: Validated.Valid[SlotData]): F[Unit] =
      (currentHead = newHead.a).pure[F]

    override def head: F[SlotData] =
      currentHead.copy().pure[F]

    override def genesis: F[SlotData] = genesisSlotData.pure[F]

    override def adoptions: F[Stream[F, BlockId]] = Stream.empty.covaryAll[F, BlockId].pure[F]
  }

  class MempoolExt[F[_]: Async](transactions: Ref[F, Set[TransactionId]]) extends MempoolAlgebra[F] {
    override def read(blockId: BlockId): F[MempoolGraph] = ???

    override def add(transactionId: TransactionId): F[Unit] = transactions.update(_ + transactionId)

    override def remove(transactionId: TransactionId): F[Unit] = transactions.update(_ - transactionId)

    override def contains(blockId: BlockId, transactionId: TransactionId): F[Boolean] = ???

    def contains(transactionId: TransactionId): F[Boolean] = transactions.get.map(_.contains(transactionId))

    def size: F[Int] = transactions.get.map(_.size)
  }

  val slotLength: FiniteDuration = FiniteDuration(200, MILLISECONDS)
  val forwardBiasedSlotWindow = 10L
  val slotsPerEpoch: Long = 100L
  val slotsPerOperationalPeriod: Long = 20L
}

class ActorPeerHandlerBridgeAlgebraTest extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  implicit val dummyDns: DnsResolver[F] = (host: HostId) => Option(host).pure[F]
  implicit val dummyReverseDns: ReverseDnsResolver[F] = (h: HostId) => h.pure[F]
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromName[F](this.getClass.getName)
  val hostId: HostId = "127.0.0.1"

  test("Start network with add one network peer: adoption shall be started, peers shall be saved in the end") {
    withMock {
      val remotePeerPort = 9085
      val remotePeerAddress = RemoteAddress("2.2.2.2", remotePeerPort)
      val remotePeer = DisconnectedPeer(remotePeerAddress, (0, 0))
      val remotePeers: List[DisconnectedPeer] = List(remotePeer)
      val hotPeersUpdatedFlag: AtomicBoolean = new AtomicBoolean(false)
      val pingProcessedFlag: AtomicBoolean = new AtomicBoolean(false)

      val localChainMock = createEmptyLocalChain

      val blockHeight = mock[EventSourcedState[F, Long => F[Option[BlockId]], BlockId]]

      val client =
        mock[BlockchainPeerClient[F]]
      (client.notifyAboutThisNetworkLevel _).expects(true).returns(Applicative[F].unit)
      (client.getPongMessage _).expects(*).anyNumberOfTimes().onCall { req: PingMessage =>
        Sync[F].delay(pingProcessedFlag.set(true)) >>
        Option(PongMessage(req.ping.reverse)).pure[F]
      }
      (client.getRemoteBlockIdAtHeight _)
        .expects(1, *)
        .returns(localChainMock.genesis.map(sd => Option(sd.slotId.blockId)))
      (() => client.remotePeer).expects().once().returns(ConnectedPeer(remotePeerAddress, (0, 0)).pure[F])

      (() => client.remotePeerAdoptions).expects().once().onCall { () =>
        Stream.fromOption[F](Option.empty[BlockId]).pure[F]
      }

      val totalTransactions = 10
      val transactions = Seq.fill(totalTransactions)(arbitraryIoTransaction.arbitrary.first).map(_.embedId)
      (() => client.remoteTransactionNotifications).expects().once().onCall { () =>
        Stream.emits(transactions.map(_.id)).covary[F].pure[F]
      }
      val transactionsMap = transactions.map(tx => tx.id -> tx).toMap
      transactions.map { _ =>
        (client
          .getRemoteTransactionOrError(_: TransactionId, _: BlockBodyOrTransactionError)(_: MonadThrow[F]))
          .expects(*, *, *)
          .once()
          .onCall {
            case (id: TransactionId, _: BlockBodyOrTransactionErrorByName @unchecked, _: MonadThrow[F] @unchecked) =>
              transactionsMap(id).pure[F]
          }
      }

      (() => client.remotePeerServerPort).expects().returns(Option(remotePeerPort).pure[F])
      (client.getRemoteKnownHosts _)
        .expects(*)
        .anyNumberOfTimes()
        .returns(Option(CurrentKnownHostsRes(Seq.empty)).pure[F])
      (client.notifyAboutThisNetworkLevel _).expects(false).returns(Applicative[F].unit)
      (client.closeConnection _).expects().returns(Applicative[F].unit)

      val peerOpenRequested: AtomicBoolean = new AtomicBoolean(false)
      val addRemotePeer: DisconnectedPeer => F[Unit] = mock[DisconnectedPeer => F[Unit]]
      (addRemotePeer.apply _).expects(remotePeer).once().returns {
        Sync[F].delay(peerOpenRequested.set(true))
      }

      var hotPeers: Set[RemoteAddress] = Set.empty
      val hotPeersUpdate: Set[RemoteAddress] => F[Unit] = mock[Set[RemoteAddress] => F[Unit]]
      (hotPeersUpdate.apply _).expects(*).anyNumberOfTimes().onCall { peers: Set[RemoteAddress] =>
        (if (peers.nonEmpty) Sync[F].delay(hotPeersUpdatedFlag.set(true)) else Applicative[F].unit) *>
        (hotPeers = peers).pure[F]
      }

      val execResource = for {
        mempool <- Ref.of[F, Set[TransactionId]](Set()).toResource
        mempoolAlgebra = new MempoolExt[F](mempool)
        slotDataStore    <- Resource.liftK(createSlotDataStore)
        headerStore      <- Resource.liftK(createHeaderStore)
        bodyStore        <- Resource.liftK(createBodyStore)
        transactionStore <- Resource.liftK(createTransactionStore)
        remotePeersStore <- Resource.liftK(createRemotePeerStore)
        blockIdTree      <- Resource.liftK(createBlockIdTree)
        localChain       <- Resource.pure(localChainMock)
        clockAlgebra <- SchedulerClock.make(
          slotLength,
          slotsPerEpoch,
          slotsPerOperationalPeriod,
          Instant.ofEpochMilli(System.currentTimeMillis()),
          forwardBiasedSlotWindow,
          () => 0L.pure[F]
        )

        topic <- Resource.make(Topic[F, PeerConnectionChange])(_.close.void)

        algebra <- ActorPeerHandlerBridgeAlgebra
          .make(
            hostId,
            localChain,
            chainSelectionAlgebra,
            headerValidation,
            headerToBodyValidation,
            defaultTxSyntaxValidator,
            bodySyntaxValidation,
            bodySemanticValidation,
            bodyAuthorizationValidation,
            slotDataStore,
            headerStore,
            bodyStore,
            transactionStore,
            remotePeersStore,
            blockIdTree,
            blockHeight,
            mempoolAlgebra,
            networkProperties,
            clockAlgebra,
            remotePeers,
            topic,
            addRemotePeer,
            hotPeersUpdate
          )
      } yield (algebra, remotePeersStore, mempoolAlgebra)

      val timeout = FiniteDuration(100, MILLISECONDS)
      for {
        ((algebra, remotePeersStore, mempoolAlgebra), algebraFinalizer) <- execResource.allocated
        _                  <- Sync[F].untilM_(Sync[F].sleep(timeout))(Sync[F].delay(peerOpenRequested.get()))
        (_, peerFinalizer) <- algebra.usePeer(client).allocated
        _                  <- Sync[F].untilM_(Sync[F].sleep(timeout))(Sync[F].delay(pingProcessedFlag.get()))
        _                  <- Sync[F].untilM_(Sync[F].sleep(timeout))(Sync[F].delay(hotPeersUpdatedFlag.get()))
        _ <- Sync[F].untilM_(Sync[F].sleep(timeout))(Sync[F].defer(mempoolAlgebra.size).map(_ == transactions.size))
        _ <- peerFinalizer
        _ <- algebraFinalizer
        _ = assert(hotPeers.contains(remotePeerAddress))
        savedPeers <- remotePeersStore.get(()).map(_.get)
        _ = assert(savedPeers.map(_.address).contains(remotePeerAddress))
        savedPeer = savedPeers.find(_.address == remotePeerAddress).get
        _ = assert(savedPeer.perfReputation != 0.0)
        txsContains <- transactions.map(_.id).traverse(mempoolAlgebra.contains)
        _ = assert(txsContains.forall(identity))
      } yield ()
    }

  }
}
