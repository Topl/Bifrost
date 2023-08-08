package co.topl.networking

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.Async
import cats.implicits._
import cats.{Applicative, Monad, MonadThrow, Show}
import co.topl.algebras.Store
import co.topl.config.ApplicationConfig.Bifrost.NetworkProperties
import co.topl.consensus.models._
import co.topl.ledger.models.{BodyAuthorizationError, BodySemanticError, BodySyntaxError, BodyValidationError}
import co.topl.networking.fsnetwork.NetworkQualityError.{IncorrectPongMessage, NoPongMessage}
import co.topl.networking.fsnetwork.ReputationAggregator.Message.PingPongMessagePing
import co.topl.networking.p2p.RemoteAddress
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import com.github.benmanes.caffeine.cache.Cache
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.util.Random

package object fsnetwork {

  type HostId = RemoteAddress // IP address? IP address could be changed and bad for identify good peer

  type HostReputationValue =
    Double // will be more complex, to get high reputation host shall fulfill different criteria

  // how many block/headers could be requested from remote host in the same time,
  // TODO shall be dynamically changed based by host reputation, i.e. bigger value for trusted host
  val chunkSize = 1

  val requestCacheSize = 100

  val blockSourceCacheSize = 512

  implicit class CacheOps[K, V](cache: Cache[K, V]) {
    def contains(key: K): Boolean = cache.getIfPresent(key) != null

    def get(key: K): Option[V] = Option(cache.getIfPresent(key))
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

  // TODO move Show instances to separate file
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
      FiniteDuration(Math.round(networkProperties.warmHostsUpdateEveryNBlock * slotDuration.toMillis), MILLISECONDS)
  }

  trait ColdToWarmSelector {
    def select(coldHosts: Set[HostId], countToReceive: Int): Set[HostId]
  }

  val RandomColdToWarmSelector: ColdToWarmSelector =
    (coldHosts: Set[HostId], countToReceive: Int) => Random.shuffle(coldHosts).take(countToReceive)

  implicit class LoggerOps[F[_]: Applicative](logger: Logger[F]) {

    def infoIf(predicate: => Boolean, message: => String): F[Unit] =
      if (predicate)
        logger.info(message)
      else
        Applicative[F].unit
  }
}
