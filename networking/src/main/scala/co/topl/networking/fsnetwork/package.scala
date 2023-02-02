package co.topl.networking

import cats.data.{EitherT, NonEmptyChain, OptionT}
import cats.implicits._
import cats.{Applicative, Monad, MonadThrow, Show}
import co.topl.algebras.Store
import co.topl.consensus.algebras.ChainSelectionAlgebra
import co.topl.consensus.models.{BlockHeaderToBodyValidationFailure, BlockHeaderValidationFailure}
import co.topl.ledger.models.{BodyAuthorizationError, BodySemanticError, BodySyntaxError}
import co.topl.models.{SlotData, TypedIdentifier}
import co.topl.networking.blockchain.BlockchainPeerClient
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

package object fsnetwork {

  type HostId = String // IP address? IP address could be changed and bad for identify good peer
  type HostReputationValue = Long // will be more complex, to get high reputation host shall fulfill different criteria

  val chunkSize = 1

  val InconsistentHeaderId =
    new IllegalArgumentException("Claimed block ID did not match provided header")

  val InconsistentTransactionId =
    new IllegalArgumentException("Claimed transaction ID did not match provided header")

  sealed trait PeerState {
    def networkLevel: Boolean

    def applicationLevel: Boolean

    def activeActor: Boolean = networkLevel || applicationLevel
  }

  object PeerState {

    case object Banned extends PeerState {
      override def networkLevel: Boolean = false

      override def applicationLevel: Boolean = false
    }

    case object Cold extends PeerState {
      override def networkLevel: Boolean = false

      override def applicationLevel: Boolean = false
    }

    case object Warm extends PeerState {
      override def networkLevel: Boolean = true

      override def applicationLevel: Boolean = false
    }

    case object Hot extends PeerState {
      override def networkLevel: Boolean = true

      override def applicationLevel: Boolean = true
    }
  }

  implicit class OptionTOps[F[_], T](optionT: OptionT[F, T]) {
    def getOrNoSuchElement(id: Any)(implicit M: MonadThrow[F]): F[T] =
      optionT.toRight(new NoSuchElementException(id.toString)).rethrowT
  }

  implicit class BlockchainPeerClientOps[F[_]: MonadThrow: Logger](client: BlockchainPeerClient[F]) {
    def getRemoteSlotDataLogged(id: TypedIdentifier): F[SlotData] =
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

  implicit class ChainSelectionAlgebraOps[F[_]: Applicative, A](algebra: ChainSelectionAlgebra[F, A]) {
    def firstIsBetter(x: A, y: A): F[Boolean] = algebra.compare(x, y).map(_ > 0)
  }

  object EitherTExt {
    def condF[F[_]: Monad, S, E](test: F[Boolean], right: => F[S], left: => F[E]): EitherT[F, E, S] = {
      EitherT.liftF(test).flatMap {
        case true => EitherT.right[E](right)
        case false => EitherT.left[S](left)
      }
    }
  }

  implicit class EitherTOps[F[_]: Monad, A, B](e: EitherT[F, A, B]) {
    def condF[S, E](test: F[Boolean], right: => S, left: => E): EitherT[F, E, S] = {
      EitherT.liftF(test).flatMap{
        case true => EitherT.right[E](right.pure[F])
        case false => EitherT.left[S](left.pure[F])
      }
    }
  }

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
  def getFromChainUntil[F[_]: Monad, T](
    getSlotDataFromT: T => F[SlotData],
    getT:             TypedIdentifier => F[T],
    terminateOn:      T => F[Boolean]
  )(from:             TypedIdentifier): F[List[T]] = {
    def iteration(acc: List[T], blockId: TypedIdentifier): F[List[T]] =
      getT(blockId).flatMap { t =>
        terminateOn(t).ifM(
          acc.pure[F],
          getSlotDataFromT(t).flatMap(slotData => iteration(acc.appended(t), slotData.parentSlotId.blockId))
        )
      }

    iteration(List.empty[T], from).map(_.reverse)
  }

  def getFirstNMissedInStore[F[_]: MonadThrow, T](
    store:     Store[F, TypedIdentifier, T],
    slotStore: Store[F, TypedIdentifier, SlotData],
    from:      SlotData,
    size:      Int
  ): OptionT[F, NonEmptyChain[TypedIdentifier]] =
    OptionT(
      getFromChainUntil(slotStore.getOrRaise, s => s.pure[F], store.contains)(from.slotId.blockId)
        .map(_.take(size))
        .map(NonEmptyChain.fromSeq)
    )
}
