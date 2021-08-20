package co.topl.consensus

import cats.data.{EitherT, OptionT}
import cats.implicits._
import co.topl.models._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.StatefulCursorChain

import scala.concurrent.{ExecutionContext, Future}

/**
 * A `StatefulCursorChain` which tracks a chain of Block Headers.  The `State` is represented as lookup methods
 * needed for the provided `ConsensusStatefullyValidatable`
 * @param latestBlockId The ID of the tip/canonical head of the chain
 * @param firstBlockId The ID of the genesis/first block ID
 * @param nextBlockId The ID of the optional child of `currentBlock`
 * @param currentBlock The block header currently pointed to by the cursor
 * @param getBlock helper method to fetch a block header
 * @param childIdOf helper method to fetch the optional child of the current block
 * @param totalStake helper method to fetch the current total stake
 * @param stakeFor helper method to fetch the stake owned by some address
 * @param epochNonce helper method to determine the current epoch nonce
 * @param append helper method to persist a new block
 * @param removeLatest helper method to rollback the latest block
 * @param consensusStatefullyValidatable helper to validate block headers
 */
case class ChainSelectionChain[StateF[_], StateFailure](
  latestBlockId: TypedIdentifier,
  firstBlockId:  TypedIdentifier,
  nextBlockId:   Option[TypedIdentifier],
  currentBlock:  BlockHeaderV2,
  getBlock:      TypedIdentifier => Future[BlockHeaderV2],
  childIdOf:     TypedIdentifier => OptionT[Future, TypedIdentifier],
  totalStake:    () => Int128,
  stakeFor:      Address => Option[Int128],
  epochNonce:    () => Nonce,
  append:        BlockHeaderV2 => Unit,
  removeLatest:  () => Unit
)(implicit ec:   ExecutionContext, consensusStatefullyValidatable: ConsensusStatefullyValidatable)
    extends StatefulCursorChain[
      Future,
      BlockHeaderV2,
      ChainSelectionChain.State[StateF, StateFailure],
      ChainSelectionChain.Failure
    ] {
  override type P = ChainSelectionChain[StateF, StateFailure]

  /**
   * Fetches the state associated with the tip of the chain
   */
  override def latestState()
    : EitherT[Future, ChainSelectionChain.Failure, ChainSelectionChain.State[StateF, StateFailure]] = ???

  /**
   * Fetch the item to which the cursor currently points
   */
  override def current: BlockHeaderV2 =
    currentBlock

  /**
   * Returns a PersistentChain in which the cursor points to the first item of the chain
   */
  override def moveFirst: Future[P] =
    for {
      firstBlock <- getBlock(firstBlockId)
      nextBlock  <- childIdOf(firstBlockId).semiflatMap(getBlock).value
    } yield copy(currentBlock = firstBlock, nextBlockId = nextBlock.map(_.id))

  /**
   * Returns a PersistentChain in which the cursor points to the tip/latest of the chain
   */
  override def moveLatest: Future[P] =
    getBlock(latestBlockId).map(h => copy(currentBlock = h, nextBlockId = None))

  /**
   * Returns a PersistentChain in which the cursor points to the previous item in the chain.
   * If no previous item exists, None is returned.
   */
  override def movePrevious: OptionT[Future, P] =
    if (currentBlock.id == firstBlockId) OptionT.none
    else
      OptionT.liftF(
        getBlock(currentBlock.parentHeaderId).map(h => copy(currentBlock = h, nextBlockId = Some(currentBlock.id)))
      )

  /**
   * Returns a PersistentChain in which the cursor points to the next item in the chain.
   * If no next item exists, None is returned.
   */
  override def moveNext: OptionT[Future, P] =
    OptionT
      .fromOption[Future](nextBlockId)
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
  override def appendToLatest(t: BlockHeaderV2): EitherT[Future, ChainSelectionChain.Failure, P] =
    if (latestBlockId != currentBlock.id)
      EitherT.liftF(moveLatest).flatMap(_.appendToLatest(t))
    else
      EitherT.fromEither(
        consensusStatefullyValidatable
          .validate(t, consensusValidationState())
          .leftMap(ChainSelectionChain.Failures.ConsensusValidationFailure)
          .map(_.header)
          .map { header =>
            append(header)
            copy(
              currentBlock = header,
              nextBlockId = None,
              latestBlockId = header.id
            )
          }
      )

  private def consensusValidationState(): ConsensusValidation.State =
    new ConsensusValidation.State {
      override def epochNonce: Nonce = ChainSelectionChain.this.epochNonce()

      override def totalStake: Int128 = ChainSelectionChain.this.totalStake()

      override def parentBlockHeader: BlockHeaderV2 = currentBlock

      override def stakeFor(address: Address): Option[Int128] = ChainSelectionChain.this.stakeFor(address)
    }

  /**
   * Points the cursor to the latest item and attempts to remove it.
   * If currently at `first`, None is instead returned. Otherwise, (the new chain, the removed value) is returned.
   */
  override def popLatest(): OptionT[Future, (P, BlockHeaderV2)] =
    if (currentBlock.id == firstBlockId) OptionT.none[Future, (P, BlockHeaderV2)]
    else if (currentBlock.id != latestBlockId) OptionT.liftF(moveLatest).flatMap(_.popLatest())
    else
      OptionT
        .liftF(getBlock(currentBlock.parentHeaderId))
        .map { parentHeader =>
          removeLatest()
          val newChain: P =
            copy(
              currentBlock = parentHeader,
              nextBlockId = None,
              latestBlockId = currentBlock.parentHeaderId
            )

          (newChain, currentBlock)
        }
}

object ChainSelectionChain {

  sealed abstract class Failure

  object Failures {
    case class ConsensusValidationFailure(failure: ConsensusValidation.Failure) extends Failure
  }

  trait State[F[_], Failure] {
    type S <: State[F, Failure]

    def epochNonce: EitherT[F, Failure, Bytes]
    def totalStake: EitherT[F, Failure, Int128]
    def stakeFor(address: TaktikosAddress): EitherT[F, Failure, Int128]

    def apply(value: BlockHeaderV2): EitherT[F, Failure, S]
    def unapply(): EitherT[F, Failure, S]
  }
}
