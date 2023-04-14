package co.topl.networking

import cats.data.{EitherT, NonEmptyChain, OptionT}
import cats.effect.Async
import cats.implicits._
import cats.{Monad, MonadThrow, Show}
import co.topl.algebras.Store
import co.topl.brambl.models.Identifier
import co.topl.consensus.models._
import co.topl.ledger.models.{BodyAuthorizationError, BodySemanticError, BodySyntaxError}
import co.topl.models.TxRoot
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.typeclasses.implicits._
import com.github.benmanes.caffeine.cache.Cache
import org.typelevel.log4cats.Logger

package object fsnetwork {

  type HostId = String // IP address? IP address could be changed and bad for identify good peer
  type HostReputationValue = Long // will be more complex, to get high reputation host shall fulfill different criteria

  // how many block/headers could be requested from remote host in the same time,
  // TODO shall be dynamically changed based by host reputation, i.e. bigger value for trusted host
  val chunkSize = 1

  val requestCacheSize = 100

  implicit class OptionTOps[F[_], T](optionT: OptionT[F, T]) {

    def getOrNoSuchElement(id: Any)(implicit M: MonadThrow[F]): F[T] =
      optionT.toRight(new NoSuchElementException(id.toString)).rethrowT
  }

  implicit class BlockchainPeerClientOps[F[_]: MonadThrow: Logger](client: BlockchainPeerClient[F]) {

    def getRemoteHeaderOrError(id: BlockId): F[BlockHeader] =
      OptionT(client.getRemoteHeader(id)).getOrRaise(BlockHeaderDownloadError.HeaderNotFoundInPeer)

    def getRemoteSlotDataLogged(id: BlockId): F[SlotData] =
      Logger[F].info(show"Fetching remote SlotData id=$id") >>
      OptionT(client.getRemoteSlotData(id)).getOrNoSuchElement(id)
  }

  object EitherTExt {

    def condF[F[_]: Monad, S, E](test: F[Boolean], ifTrue: => F[S], ifFalse: => F[E]): EitherT[F, E, S] =
      EitherT.liftF(test).flatMap {
        case true  => EitherT.right[E](ifTrue)
        case false => EitherT.left[S](ifFalse)
      }
  }

  implicit class CacheOps[K, V](cache: Cache[K, V]) {
    def contains(key: K): Boolean = cache.getIfPresent(key) != null

    def get(key: K): Option[V] = Option(cache.getIfPresent(key))
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

  // Return A1 -> A2 -> ... -> AN -> from, where terminateOn(A1) === true

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

  def dropKnownPrefix[F[_]: Async, T](
    data:  Seq[(BlockId, T)],
    store: Store[F, BlockId, T]
  ): F[Option[NonEmptyChain[(BlockId, T)]]] =
    data.dropWhileF(idAndBody => store.contains(idAndBody._1)).map(NonEmptyChain.fromSeq)

  sealed trait BlockBodyDownloadError extends Exception

  object BlockBodyDownloadError {

    case object BodyNotFoundInPeer extends BlockBodyDownloadError {
      override def toString: String = "Block body has not found in peer"
    }

    case class BodyHaveIncorrectTxRoot(headerTxRoot: TxRoot, bodyTxRoot: TxRoot) extends BlockBodyDownloadError {
      override def toString: String = show"Peer returns body with bad txRoot: expected $headerTxRoot, got $bodyTxRoot"
    }

    case class TransactionNotFoundInPeer(transactionId: Identifier.IoTransaction32) extends BlockBodyDownloadError {
      override def toString: String = show"Peer have no transaction $transactionId despite having appropriate block"
    }

    case class TransactionHaveIncorrectId(expected: Identifier.IoTransaction32, actual: Identifier.IoTransaction32)
        extends BlockBodyDownloadError {
      override def toString: String = show"Peer returns transaction with bad id: expected $expected, actual $actual"
    }

    case class UnknownError(ex: Throwable) extends BlockBodyDownloadError {
      this.initCause(ex)

      override def toString: String = {
        val name = Option(ex.getClass.getName).getOrElse("")
        val message = Option(ex.getLocalizedMessage).getOrElse("")
        s"Unknown error during getting block from peer due next throwable $name : $message"
      }
    }
  }

  sealed trait BlockHeaderDownloadError extends Exception

  object BlockHeaderDownloadError {

    case object HeaderNotFoundInPeer extends BlockBodyDownloadError {
      override def toString: String = "Block body has not found in peer"
    }

    case class HeaderHaveIncorrectId(expected: BlockId, actual: BlockId) extends BlockBodyDownloadError {
      override def toString: String = show"Peer returns header with bad id: expected $expected, actual $actual"
    }

    case class UnknownError(ex: Throwable) extends BlockHeaderDownloadError {
      this.initCause(ex)

      override def toString: String = {
        val name = Option(ex.getClass.getName).getOrElse("")
        val message = Option(ex.getLocalizedMessage).getOrElse("")
        s"Unknown error during getting header from peer due next throwable $name : $message"
      }
    }
  }
}
