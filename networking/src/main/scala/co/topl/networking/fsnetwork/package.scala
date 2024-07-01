package co.topl.networking

import cats.data.{Chain, NonEmptyChain, OptionT}
import cats.effect.Async
import cats.implicits._
import cats.{Applicative, Monad, MonadThrow}
import co.topl.algebras.Store
import co.topl.codecs.bytes.scodecs.valuetypes._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models._
import co.topl.models.p2p._
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.node.models.BlockBody
import com.github.benmanes.caffeine.cache.Cache
import org.typelevel.log4cats.Logger
import scodec.Codec
import scodec.codecs.{cstring, double, int32, vlong}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

package object fsnetwork {

  val hostIdBytesLen: Int = 32

  val requestCacheSize = 256
  val slotIdResponseCacheSize = 256

  val blockSourceCacheSize = 1024

  // magic number will be refactored later
  val peerSlotDataStoreCacheSize = 20000

  // requests in proxy shall be expired, there is no strict guarantee that we will receive responses
  val proxyBlockDataTTL: FiniteDuration = 1.seconds
  val proxySlotDataTTL: FiniteDuration = 30.seconds

  type CommonAncestorF[F[_]] = (BlockchainPeerClient[F], LocalChainAlgebra[F]) => F[BlockId]

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

  implicit class NonEmptyChainFOps[T, F[_]: Async](seq: NonEmptyChain[T]) {

    def dropWhileF(p: T => F[Boolean]): F[Chain[T]] = {
      def go(rem: Chain[T]): F[Chain[T]] =
        rem.uncons match {
          case Some((a, tail)) =>
            p(a).ifM(go(tail), rem.pure[F])
          case None => Chain.empty[T].pure[F]
        }

      go(seq.toChain)
    }
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
    client:     BlockchainPeerClient[F],
    localChain: LocalChainAlgebra[F]
  ): F[BlockId] =
    client
      .findCommonAncestor(
        height =>
          OptionT(localChain.blockIdAtHeight(height))
            .getOrRaise(new IllegalArgumentException(s"Unknown height=$height")),
        () => localChain.head.map(_.height)
      )

  /**
   * Get some T from chain until reach terminateOn condition, f.e.
   * let assume we have chain:
   * A0 -> A1 -> ... -> A(N-1) -> AN -> A(N+1), where T(A0) appropriate data T for id A0,
   * then if terminateOn(T(A(N-3))) == true AND from === A(N) then function return chain
   * List(T(A(N-3)), T(A(N-2)), T(A(N-1)), T(A(N)))
   * @param getSlotDataFromT define how slot data could be obtained for T
   * @param getT define how T could be get by Id
   * @param terminateOn terminate condition
   * @param last start point to process chain
   * @tparam F effect
   * @tparam T type of data retrieved from chain
   * @return chain with data T
   */
  def prependOnChainUntil[F[_]: Monad, T](
    getSlotDataFromT: T => F[SlotData],
    getT:             BlockId => F[T],
    terminateOn:      T => F[Boolean]
  )(last: BlockId): F[List[T]] = {
    def iteration(acc: Chain[T], blockId: BlockId): F[Chain[T]] =
      getT(blockId).flatMap { t =>
        terminateOn(t).ifM(
          acc.pure[F],
          getSlotDataFromT(t).flatMap(slotData => iteration(acc.append(t), slotData.parentSlotId.blockId))
        )
      }

    iteration(Chain.empty[T], last).map(_.toList.reverse)
  }

  /**
   * build first "size" elements missed in store
   * @param store store to be checked, i.e. first "size" element absent in that store are returned
   * @param getSlotDataFetcher slot data fetcher
   * @param from start point to check
   * @param size maximum size of returned elements
   * @tparam F effect
   * @tparam T type of data
   * @return missed ids for "store"
   */
  def getFirstNMissedInStore[F[_]: MonadThrow, T](
    store:              Store[F, BlockId, T],
    getSlotDataFetcher: BlockId => F[SlotData],
    from:               BlockId,
    size:               Int
  ): OptionT[F, NonEmptyChain[BlockId]] =
    OptionT(
      prependOnChainUntil(getSlotDataFetcher, s => s.pure[F], store.contains)(from)
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
    peerId:  HostId,
    address: RemoteAddress
  )

  private val hostIdCodec: Codec[HostId] = byteStringCodec.as[HostId]
  private val remoteAddressCodec: Codec[RemoteAddress] = (cstring :: int32).as[RemoteAddress]

  implicit val peerToAddCodec: Codec[KnownRemotePeer] =
    (hostIdCodec :: remoteAddressCodec :: double :: double :: optionCodec[Long](vlong)).as[KnownRemotePeer]

  implicit class LoggerOps[F[_]: Applicative](logger: Logger[F]) {

    def infoIf(predicate: => Boolean, message: => String): F[Unit] =
      if (predicate)
        logger.info(message)
      else
        Applicative[F].unit
  }
}
