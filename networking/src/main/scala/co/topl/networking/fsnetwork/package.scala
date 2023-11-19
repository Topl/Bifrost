package co.topl.networking

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.{Async, Sync}
import cats.implicits._
import cats.{Applicative, Monad, MonadThrow, Show}
import co.topl.algebras.Store
import co.topl.brambl.validation.TransactionSyntaxError
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models._
import co.topl.eventtree.EventSourcedState
import co.topl.ledger.models.{BodyAuthorizationError, BodySemanticError, BodySyntaxError, BodyValidationError}
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.PeersManager.Message.PingPongMessagePing
import co.topl.networking.p2p.RemoteAddress
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import com.comcast.ip4s.{Dns, Hostname, IpAddress}
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import org.typelevel.log4cats.Logger
import scalacache.Entry
import scalacache.caffeine.CaffeineCache
import scodec.Codec
import scodec.codecs.{cstring, double, int32}

import java.time.Duration
import scala.concurrent.duration.{DurationInt, FiniteDuration, MILLISECONDS}

package object fsnetwork {

  type HostId = String // IP address? IP address could be changed and bad for identify good peer

  type HostReputationValue =
    Double // will be more complex, to get high reputation host shall fulfill different criteria

  // how many block/headers could be requested from remote host in the same time,
  // TODO shall be dynamically changed based by host reputation, i.e. bigger value for trusted host
  val chunkSize = 1

  val requestCacheSize = 100

  val blockSourceCacheSize = 512

  type BlockHeights[F[_]] = EventSourcedState[F, Long => F[Option[BlockId]], BlockId]

  implicit class CacheOps[K, V](cache: Cache[K, V]) {
    def contains(key: K): Boolean = cache.getIfPresent(key) != null

    def get(key: K): Option[V] = Option(cache.getIfPresent(key))

    def getOrElse[B >: V](key: K, default: => V): V =
      get(key).getOrElse(default)
  }

  implicit class StreamOps[T, F[_]: Async](stream: fs2.Stream[F, T]) {

    def evalDropWhile(p: T => F[Boolean]): fs2.Stream[F, T] =
      stream.evalMap(a => p(a).map(b => (b, a))).dropWhile(_._1).map(_._2)
  }

  implicit class SeqFOps[T, F[_]: Async](seq: Seq[T]) {

    def takeWhileF(p: T => F[Boolean]): F[List[T]] =
      fs2.Stream
        .emits(seq)
        .evalMap(data => p(data).map((data, _)))
        .takeWhile(_._2)
        .map(_._1)
        .compile
        .toList

    def dropWhileF(p: T => F[Boolean]): F[List[T]] =
      fs2.Stream
        .emits(seq)
        .evalMap(data => p(data).map((data, _)))
        .dropWhile(_._2)
        .map(_._1)
        .compile
        .toList
  }

  def commonAncestor[F[_]: Async: Logger](
    client:       BlockchainPeerClient[F],
    blockHeights: BlockHeights[F],
    localChain:   LocalChainAlgebra[F]
  ): F[BlockId] =
    client
      .findCommonAncestor(
        getLocalBlockIdAtHeight(localChain, blockHeights),
        () => localChain.head.map(_.height)
      )

  private def getLocalBlockIdAtHeight[F[_]: Async](
    localChain:   LocalChainAlgebra[F],
    blockHeights: BlockHeights[F]
  )(height: Long): F[BlockId] =
    OptionT(
      localChain.head
        .map(_.slotId.blockId)
        .flatMap(blockHeights.useStateAt(_)(_.apply(height)))
    ).toRight(new IllegalStateException("Unable to determine block height tree")).rethrowT

  // TODO move Show instances to separate file
  implicit val showTransactionSyntaxError: Show[TransactionSyntaxError] =
    Show.fromToString

  implicit val showBlockHeaderValidationFailure: Show[BlockHeaderValidationFailure] =
    Show.fromToString

  implicit val showBodySyntaxError: Show[BodySyntaxError] =
    Show.fromToString

  implicit val showBodySemanticError: Show[BodySemanticError] =
    Show.fromToString

  implicit val showBodyAuthorizationError: Show[BodyAuthorizationError] =
    Show.fromToString

  implicit val showHeaderToBodyError: Show[BlockHeaderToBodyValidationFailure] =
    Show.fromToString

  implicit val showBodyValidationError: Show[BodyValidationError] =
    Show.fromToString

  implicit val showNoPongMessage: Show[NetworkQualityError] = {
    case _: NoPongMessage.type        => "Failed to receive pong message"
    case _: IncorrectPongMessage.type => "Receive incorrect pong message"
  }

  implicit val showPongMessage: Show[PingPongMessagePing] = {
    case PingPongMessagePing(host, Right(delay))       => show"Received pong delay $delay from host $host"
    case PingPongMessagePing(host, Left(networkError)) => show"$networkError from host $host"
  }

  implicit val showPeerState: Show[PeerState] = {
    case PeerState.Banned  => "BANNED"
    case PeerState.Cold    => "COLD"
    case PeerState.Warm    => "WARM"
    case PeerState.Hot     => "HOT"
    case PeerState.Unknown => "UNKNOWN"
  }

  implicit def showPeer[F[_]]: Show[Peer[F]] = { peer: Peer[F] =>
    "Peer" +
    s" ${peer.ip}:[${peer.remoteServerPort.map(_.toString).getOrElse("")}];" +
    s" State is ${peer.state.toString};" +
    s" Actor is ${if (peer.actorOpt.isDefined) "present" else "absent"};" +
    s" Remote peer is ${if (peer.remoteNetworkLevel) "active" else "no active"};" +
    s" Reputation is: block=${peer.blockRep}, perf=${peer.perfRep}, new=${peer.newRep}, mean=${peer.reputation};" +
    s" With total ${peer.closedTimestamps.size} closes with timestamps ${peer.closedTimestamps}"
  }

  implicit val remotePeerShow: Show[RemotePeer] = { remotePeer: RemotePeer =>
    s"Remote peer: ${remotePeer.address}"
  }

  /**
   * Get some T from chain until reach terminateOn condition, f.e.
   * let assume we have chain:
   * A0 -> A1 -> ... -> A(N-1) -> AN -> A(N+1), where T(A0) appropriate data T for id A0,
   * then if terminateOn(T(A(N-3))) == true AND from === A(N) then function return chain
   * List(T(A(N-3)), T(A(N-2)), T(A(N-1)), T(A(N)))
   * @param getSlotDataFromT define how slot data could be obtained for T
   * @param getT define how T could be get by Id
   * @param terminateOn terminate condition
   * @param from start point to process chain
   * @tparam F effect
   * @tparam T type of data retrieved from chain
   * @return chain with data T
   */
  def getFromChainUntil[F[_]: Monad, T](
    getSlotDataFromT: T => F[SlotData],
    getT:             BlockId => F[T],
    terminateOn:      T => F[Boolean]
  )(from: BlockId): F[List[T]] = {
    def iteration(acc: List[T], blockId: BlockId): F[List[T]] =
      getT(blockId).flatMap { t =>
        terminateOn(t).ifM(
          acc.pure[F],
          getSlotDataFromT(t).flatMap(slotData => iteration(acc.appended(t), slotData.parentSlotId.blockId))
        )
      }

    iteration(List.empty[T], from).map(_.reverse)
  }

  /**
   * build first "size" elements missed in store
   * @param store store to be checked, i.e. first "size" element absent in that store are returned
   * @param slotStore slot store
   * @param from start point to check
   * @param size maximum size of returned elements
   * @tparam F effect
   * @tparam T type of data
   * @return missed ids for "store"
   */
  def getFirstNMissedInStore[F[_]: MonadThrow, T](
    store:     Store[F, BlockId, T],
    slotStore: Store[F, BlockId, SlotData],
    from:      BlockId,
    size:      Int
  ): OptionT[F, NonEmptyChain[BlockId]] =
    OptionT(
      getFromChainUntil(slotStore.getOrRaise, s => s.pure[F], store.contains)(from)
        .map(_.take(size))
        .map(NonEmptyChain.fromSeq)
    )

  def dropKnownPrefix[F[_]: Async, I, T, D](data: Seq[(I, D)], store: Store[F, BlockId, T])(
    iToId: I => BlockId
  ): F[Option[NonEmptyChain[(I, D)]]] =
    data.dropWhileF(d => store.contains(iToId(d._1))).map(NonEmptyChain.fromSeq)

  case class UnverifiedBlockHeader(source: HostId, blockHeader: BlockHeader, downloadTimeMs: Long)

  case class UnverifiedBlockBody(
    source:           HostId,
    blockBody:        BlockBody,
    downloadTimeMs:   Long,
    downloadTimeTxMs: Seq[Long] = Seq.empty
  )

  case class RemotePeer(
    address:         RemoteAddress,
    blockReputation: HostReputationValue,
    perfReputation:  HostReputationValue
  )

  private val remoteAddressCodec: Codec[RemoteAddress] = (cstring :: int32).as[RemoteAddress]
  implicit val peerToAddCodec: Codec[RemotePeer] = (remoteAddressCodec :: double :: double).as[RemotePeer]

  case class P2PNetworkConfig(networkProperties: NetworkProperties, slotDuration: FiniteDuration) {

    /**
     * Block providing novelty reputation if new unknown block is received in current slot.
     * Shall be always equal one.
     */
    val blockNoveltyInitialValue: Double = 1

    /**
     * Reducing block novelty reputation for each already known source, i.e:
     * blockNoveltyReputation = 1 - knewSourceForThatBlockId * blockNoveltyReputationStep
     */
    val blockNoveltyReputationStep: Double = 0.2

    /**
     * Block novelty reputation shall be reducing every slot by X number.
     * If we have reputation of "blockNoveltyInitialValue" then after "expectedSlotsPerBlock" slots that
     * reputation shall be equal to "blockNoveltyInitialValue" - "blockNoveltyReputationStep".
     * Thus we need such X number where:
     *  pow(X, expectedSlotsPerBlock - 1) == "blockNoveltyInitialValue" - blockNoveltyReputationStep,
     * then:
     *  X = root of (1 - blockNoveltyReputationStep) with index (expectedSlotsPerBlock - 1)
     */
    val blockNoveltyDecoy: Double =
      Math.pow(blockNoveltyInitialValue - blockNoveltyReputationStep, 1 / (networkProperties.expectedSlotsPerBlock - 1))

    /**
     * Maximum possible performance reputation, i.e. reputation for host with delay in 0 ms
     */
    val performanceReputationInitialValue: Double = 1

    /**
     * Any remote peer with "ping" equal or more than performanceReputationMaxDelay will have 0 performance reputation
     */
    val performanceReputationMaxDelay: Double = slotDuration.toMillis * networkProperties.maxPerformanceDelayInSlots

    /**
     * New remote peer will not be closed during "remotePeerNoveltyInSlots" slots even if reputation is low.
     * It gives a chance to build-up reputation for remote peer
     */
    val remotePeerNoveltyInSlots: Long =
      Math.ceil(networkProperties.expectedSlotsPerBlock * networkProperties.remotePeerNoveltyInExpectedBlocks).toLong

    /**
     * How often we update our list of warm hosts
     */
    val warmHostsUpdateInterval: FiniteDuration =
      FiniteDuration(
        Math.ceil(networkProperties.warmHostsUpdateEveryNBlock * slotDuration.toMillis).toInt,
        MILLISECONDS
      )

    val aggressiveP2PRequestInterval: FiniteDuration = remotePeerNoveltyInSlots * slotDuration
  }

  implicit class LoggerOps[F[_]: Applicative](logger: Logger[F]) {

    def infoIf(predicate: => Boolean, message: => String): F[Unit] =
      if (predicate)
        logger.info(message)
      else
        Applicative[F].unit
  }

  trait DnsResolver[F[_]] {
    def resolving(host: HostId): F[Option[HostId]]
  }

  object DnsResolverInstances {

    class DefaultDnsResolver[F[_]: Dns: Sync] extends DnsResolver[F] {
      val dnsCacheSize: Int = 1000
      val expireAfterWriteDuration: Duration = java.time.Duration.ofMinutes(30)

      val cache: Cache[HostId, HostId] =
        Caffeine.newBuilder
          .maximumSize(dnsCacheSize)
          .expireAfterWrite(expireAfterWriteDuration)
          .build[String, String]()

      override def resolving(host: HostId): F[Option[HostId]] =
        if (cache.contains(host)) {
          Option(cache.getIfPresent(host)).pure[F]
        } else {
          doResolve(host).flatTap(hostname => hostname.traverse(rh => Sync[F].delay(cache.put(host, rh))))
        }

      private def doResolve(unresolvedHost: HostId): F[Option[HostId]] = {
        val resolver: Dns[F] = implicitly[Dns[F]]

        val res =
          for {
            host     <- OptionT.fromOption[F](Hostname.fromString(unresolvedHost))
            resolved <- OptionT(resolver.resolveOption(host))
          } yield resolved.toUriString
        res.value
      }
    }
  }

  implicit class DnsResolverSyntax[F[_]](hostId: HostId)(implicit val resolver: DnsResolver[F]) {
    def resolving(): F[Option[HostId]] = resolver.resolving(hostId)
  }

  trait DnsResolverHT[T, F[_]] {
    def resolving(host: T): F[Option[T]]
  }

  object DnsResolverHTInstances {

    implicit def remoteAddressResolver[F[_]: Monad: DnsResolver]: DnsResolverHT[RemoteAddress, F] =
      (unresolvedHost: RemoteAddress) => {
        val resolver = implicitly[DnsResolver[F]]
        resolver.resolving(unresolvedHost.host).map(_.map(resolved => unresolvedHost.copy(host = resolved)))
      }

    implicit def peerToAddResolver[F[_]: Monad: DnsResolver]: DnsResolverHT[RemotePeer, F] =
      (unresolvedHost: RemotePeer) => {
        val resolver = implicitly[DnsResolverHT[RemoteAddress, F]]
        resolver.resolving(unresolvedHost.address).map(_.map(resolved => unresolvedHost.copy(address = resolved)))
      }
  }

  implicit class DnsResolverHTSyntax[F[_], T](host: T)(implicit res: DnsResolverHT[T, F]) {

    def resolving(): F[Option[T]] = {
      val resolver: DnsResolverHT[T, F] = implicitly[DnsResolverHT[T, F]]
      resolver.resolving(host)
    }
  }

  trait ReverseDnsResolver[F[_]] {
    def reverseResolving(host: HostId): F[HostId]
  }

  object ReverseDnsResolverInstances {

    class NoOpReverseResolver[F[_]: Applicative] extends ReverseDnsResolver[F] {
      override def reverseResolving(host: HostId): F[HostId] = host.pure[F]
    }

    class DefaultReverseDnsResolver[F[_]: Dns: Sync] extends ReverseDnsResolver[F] {
      val reverseDnsCacheSize: Int = 1000
      val expireAfterWriteDuration: FiniteDuration = 30.minutes

      val cache: CaffeineCache[F, String, String] =
        CaffeineCache[F, String, String](
          Caffeine.newBuilder
            .maximumSize(reverseDnsCacheSize)
            .build[String, Entry[String]]()
        )

      override def reverseResolving(hostIdAsIp: HostId): F[HostId] =
        cache.cachingF(hostIdAsIp)(expireAfterWriteDuration.some)(doResolve(hostIdAsIp))

      private def doResolve(hostIdAsIp: HostId): F[HostId] = {
        val resolver: Dns[F] = implicitly[Dns[F]]
        val res =
          for {
            ip       <- OptionT.fromOption[F](IpAddress.fromString(hostIdAsIp))
            resolved <- OptionT(resolver.reverseOption(ip))
          } yield resolved.normalized.toString
        // if we failed to get hostname then still use ip
        res.value.map(_.getOrElse(hostIdAsIp))
      }
    }
  }

  implicit class ReverseDnsResolverSyntax[F[_]](hostId: HostId)(implicit val resolver: ReverseDnsResolver[F]) {
    def reverseResolving(): F[HostId] = resolver.reverseResolving(hostId)
  }

  trait ReverseDnsResolverHT[T, F[_]] {
    def reverseResolving(host: T): F[T]
  }

  object ReverseDnsResolverHTInstances {

    implicit def reverseRemoteAddressResolver[F[_]: Monad: ReverseDnsResolver]: ReverseDnsResolverHT[RemoteAddress, F] =
      (resolvedHost: RemoteAddress) => {
        val resolver = implicitly[ReverseDnsResolver[F]]
        resolver.reverseResolving(resolvedHost.host).map(resolved => resolvedHost.copy(host = resolved))
      }

    implicit def reversePeerToAddResolver[F[_]: Monad: ReverseDnsResolver]: ReverseDnsResolverHT[RemotePeer, F] =
      (resolvedHost: RemotePeer) => {
        val resolver = implicitly[ReverseDnsResolverHT[RemoteAddress, F]]
        resolver.reverseResolving(resolvedHost.address).map(resolved => resolvedHost.copy(address = resolved))
      }
  }

  implicit class ReverseDnsResolverHTSyntax[F[_], T](host: T)(implicit res: ReverseDnsResolverHT[T, F]) {

    def reverseResolving(): F[T] = {
      val resolver: ReverseDnsResolverHT[T, F] = implicitly[ReverseDnsResolverHT[T, F]]
      resolver.reverseResolving(host)
    }
  }
}
