package co.topl.networking

import cats.data.{NonEmptyChain, OptionT}
import cats.effect.Async
import cats.implicits._
import cats.{Applicative, Monad, MonadThrow}
import co.topl.algebras.Store
import co.topl.codecs.bytes.scodecs.valuetypes._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.consensus.models._
import co.topl.eventtree.EventSourcedState
import co.topl.models.Bytes
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.networking.p2p.RemoteAddress
import co.topl.node.models.BlockBody
import co.topl.typeclasses.implicits._
import com.github.benmanes.caffeine.cache.Cache
import org.typelevel.log4cats.Logger
import scodec.Codec
import scodec.codecs.{cstring, double, int32}

package object fsnetwork {

  val hostIdBytesLen: Int = 32
  case class HostId(id: Bytes) extends AnyVal

  type HostReputationValue =
    Double // will be more complex, to get high reputation host shall fulfill different criteria

  // how many block/headers could be requested from remote host in the same time,
  // TODO shall be dynamically changed based by host reputation, i.e. bigger value for trusted host
  val chunkSize = 1

  val requestCacheSize = 256

  val bodyStoreContainsCacheSize = 32768
  val blockSourceCacheSize = 1024

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
    peerId:  HostId,
    address: RemoteAddress
  )

  case class KnownRemotePeer(
    peerId:          HostId,
    address:         RemoteAddress,
    blockReputation: HostReputationValue,
    perfReputation:  HostReputationValue
  )

  private val hostIdCodec: Codec[HostId] = byteStringCodec.as[HostId]
  private val remoteAddressCodec: Codec[RemoteAddress] = (cstring :: int32).as[RemoteAddress]

  implicit val peerToAddCodec: Codec[KnownRemotePeer] =
    (hostIdCodec :: remoteAddressCodec :: double :: double).as[KnownRemotePeer]

  implicit class LoggerOps[F[_]: Applicative](logger: Logger[F]) {

    def infoIf(predicate: => Boolean, message: => String): F[Unit] =
      if (predicate)
        logger.info(message)
      else
        Applicative[F].unit
  }
}
