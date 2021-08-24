package co.topl.consensus

import cats.Monad
import cats.data.{EitherT, OptionT}
import cats.implicits._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models._
import co.topl.models.utility.{Lengths, Sized}
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.StatefulCursorChain

/**
 * A `StatefulCursorChain` which tracks a chain of Block Headers.  The `State` is represented as lookup methods
 * needed for the provided `ConsensusStatefullyValidatable`
 * @param latestBlockId The ID of the tip/canonical head of the chain
 * @param firstBlockId The ID of the genesis/first block ID
 * @param nextBlockId The ID of the optional child of `currentBlock`
 * @param currentBlock The block header currently pointed to by the cursor
 * @param getBlock helper method to fetch a block header
 * @param childIdOf helper method to fetch the optional child of the current block
 * @param relativeStakeFor helper method to fetch the relative stake owned by some address
 * @param epochNonce helper method to determine the current epoch nonce
 * @param append helper method to persist a new block
 * @param removeLatest helper method to rollback the latest block
 * @param consensusStatefullyValidatable helper to validate block headers
 */
case class ChainSelectionChain[F[_]: Monad, StateFailure](
  latestBlockId:                           TypedIdentifier,
  firstBlockId:                            TypedIdentifier,
  nextBlockId:                             Option[TypedIdentifier],
  currentBlock:                            BlockHeaderV2,
  getBlock:                                TypedIdentifier => F[BlockHeaderV2],
  childIdOf:                               TypedIdentifier => OptionT[F, TypedIdentifier],
  state:                                   ChainSelectionChain.State[F, StateFailure]
)(implicit consensusStatefullyValidatable: ConsensusStatefullyValidatable.type)
    extends StatefulCursorChain[
      F,
      BlockHeaderV2,
      ChainSelectionChain.State[F, StateFailure],
      ChainSelectionChain.Failure
    ] {
  override type P = ChainSelectionChain[F, StateFailure]

  private val ZeroStake = Sized.max[BigInt, Lengths.`128`.type](0: BigInt)

  /**
   * Fetches the state associated with the tip of the chain
   */
  override def latestState(): EitherT[F, ChainSelectionChain.Failure, ChainSelectionChain.State[F, StateFailure]] =
    EitherT.pure[F, ChainSelectionChain.Failure](state)

  /**
   * Fetch the item to which the cursor currently points
   */
  override def current: BlockHeaderV2 =
    currentBlock

  /**
   * Returns a PersistentChain in which the cursor points to the first item of the chain
   */
  override def moveFirst: F[P] =
    for {
      firstBlock <- getBlock(firstBlockId)
      nextBlock  <- childIdOf(firstBlockId).semiflatMap(getBlock).value
    } yield copy(currentBlock = firstBlock, nextBlockId = nextBlock.map(_.id))

  /**
   * Returns a PersistentChain in which the cursor points to the tip/latest of the chain
   */
  override def moveLatest: F[P] =
    getBlock(latestBlockId).map(h => copy(currentBlock = h, nextBlockId = None))

  /**
   * Returns a PersistentChain in which the cursor points to the previous item in the chain.
   * If no previous item exists, None is returned.
   */
  override def movePrevious: OptionT[F, P] =
    if (currentBlock.id == firstBlockId) OptionT.none
    else
      OptionT.liftF(
        getBlock(currentBlock.parentHeaderId).map(h => copy(currentBlock = h, nextBlockId = Some(currentBlock.id)))
      )

  /**
   * Returns a PersistentChain in which the cursor points to the next item in the chain.
   * If no next item exists, None is returned.
   */
  override def moveNext: OptionT[F, P] =
    OptionT
      .fromOption[F](nextBlockId)
      .flatMap(nextBlockId =>
        OptionT
          .liftF(getBlock(nextBlockId))
          .semiflatMap(nextBlock =>
            childIdOf(nextBlockId).value
              .map(nextNextBlockId => copy(currentBlock = nextBlock, nextBlockId = nextNextBlockId))
          )
      )

  /**
   * Points the cursor to the latest item and attempts to append the given item T to it.
   * If non-applicable or invalid, a Failure is returned
   */
  override def appendToLatest(t: BlockHeaderV2): EitherT[F, ChainSelectionChain.Failure, P] =
    if (latestBlockId != currentBlock.id)
      EitherT.liftF(moveLatest).flatMap(_.appendToLatest(t))
    else
      EitherT
        .fromEither[F](
          consensusStatefullyValidatable
            .validate(t, consensusValidationState())
            .leftMap(ChainSelectionChain.Failures.ConsensusValidationFailure)
            .map(_.header)
        )
        .flatMap { header =>
          state
            .apply(header)
            .leftMap(ChainSelectionChain.Failures.StateFailure(_): ChainSelectionChain.Failure)
            .map(newState =>
              copy(
                currentBlock = header,
                nextBlockId = None,
                latestBlockId = header.id,
                state = newState
              )
            )
        }

  private def consensusValidationState(): ConsensusValidation.State =
    new ConsensusValidation.State {
      override def epochNonce: Nonce = ???

      override def totalStake: Int128 = ???

      override def parentBlockHeader: BlockHeaderV2 = currentBlock

      override def stakeFor(address: Address): Option[Int128] = ???
    }

  /**
   * Points the cursor to the latest item and attempts to remove it.
   * If currently at `first`, None is instead returned. Otherwise, (the new chain, the removed value) is returned.
   */
  override def popLatest(): OptionT[F, (P, BlockHeaderV2)] =
    if (currentBlock.id == firstBlockId) OptionT.none[F, (P, BlockHeaderV2)]
    else if (currentBlock.id != latestBlockId) OptionT.liftF(moveLatest).flatMap(_.popLatest())
    else
      OptionT
        .liftF(getBlock(currentBlock.parentHeaderId))
        .flatMap { parentHeader =>
          state
            .unapply()
            .toOption
            .map(newState =>
              copy(
                currentBlock = parentHeader,
                nextBlockId = None,
                latestBlockId = currentBlock.parentHeaderId,
                state = newState
              )
            )
            .map(_ -> currentBlock)
        }
}

object ChainSelectionChain {

  sealed abstract class Failure

  object Failures {
    case class ConsensusValidationFailure(failure: ConsensusValidation.Failure) extends Failure
    case class StateFailure[Reason](failure: Reason) extends Failure
  }

  trait State[F[_], Failure] {
    type S <: State[F, Failure]

    def epochNonce: EitherT[F, Failure, Bytes]
    def relativeStakeFor(address: TaktikosAddress): EitherT[F, Failure, Int128]

    def apply(value: BlockHeaderV2): EitherT[F, Failure, S]
    def unapply(): EitherT[F, Failure, S]
  }
}
