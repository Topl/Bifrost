package co.topl.networking

import cats.data.{EitherT, NonEmptyChain, OptionT}
import cats.implicits._
import cats.{Applicative, Monad, MonadThrow, Show}
import co.topl.algebras.Store
import co.topl.consensus.models.{BlockHeaderToBodyValidationFailure, BlockHeaderValidationFailure, BlockId, SlotData}
import co.topl.ledger.models.{BodyAuthorizationError, BodySemanticError, BodySyntaxError}
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

package object fsnetwork {

  type HostId = String // IP address? IP address could be changed and bad for identify good peer
  type HostReputationValue = Long // will be more complex, to get high reputation host shall fulfill different criteria

  // how many block/headers could be requested from remote host in the same time,
  // TODO shall be dynamically changed based by host reputation, i.e. bigger value for trusted host
  val chunkSize = 1

  implicit class OptionTOps[F[_], T](optionT: OptionT[F, T]) {

    def getOrNoSuchElement(id: Any)(implicit M: MonadThrow[F]): F[T] =
      optionT.toRight(new NoSuchElementException(id.toString)).rethrowT
  }

  implicit class BlockchainPeerClientOps[F[_]: MonadThrow: Logger](client: BlockchainPeerClient[F]) {

    def getRemoteSlotDataLogged(id: BlockId): F[SlotData] =
      Logger[F].info(show"Fetching remote SlotData id=$id") >>
      OptionT(client.getRemoteSlotData(id)).getOrNoSuchElement(id)
  }

  implicit class StoreOps[F[_]: Applicative, Key, T](store: Store[F, Key, T]) {

    def filterKnown(key: Key, value: T): F[Option[(Key, T)]] =
      store.contains(key).map {
        case true  => None
        case false => Option(key, value)
      }

    def filterKnownBlocks(blocks: NonEmptyChain[(Key, T)]): F[Option[NonEmptyChain[(Key, T)]]] =
      blocks.toList
        .traverse { case (key, value) => store.filterKnown(key, value) }
        .map(_.flatten)
        .map(NonEmptyChain.fromSeq)
  }

  object EitherTExt {

    def condF[F[_]: Monad, S, E](test: F[Boolean], ifTrue: => F[S], ifFalse: => F[E]): EitherT[F, E, S] =
      EitherT.liftF(test).flatMap {
        case true  => EitherT.right[E](ifTrue)
        case false => EitherT.left[S](ifFalse)
      }
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
          getSlotDataFromT(t).flatMap { slotData =>
            iteration(acc.appended(t), slotData.parentSlotId.blockId)
          }
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
    from:      SlotData,
    size:      Int
  ): OptionT[F, NonEmptyChain[BlockId]] =
    OptionT(
      getFromChainUntil(slotStore.getOrRaise, s => s.pure[F], store.contains)(from.slotId.blockId)
        .map(_.take(size))
        .map(NonEmptyChain.fromSeq)
    )

  sealed trait BlockBodyDownloadError
  object BlockBodyDownloadError {}
}
