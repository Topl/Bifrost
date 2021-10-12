package co.topl.consensus

import akka.util.Timeout
import cats.data.{NonEmptySet, OptionT}
import cats.effect.Async
import co.topl.models._
import cats.implicits._
import cats.kernel.Order
import co.topl.typeclasses.ContainsSlotId
import co.topl.typeclasses.implicits._

import scala.collection.immutable.SortedSet

/**
 * A Tine is a reverse-singly-linked-list of Block Headers that can be traced back to genesis (or some trunk)
 *
 * @example ((CanonicalTine <- BlockTine) <- BlockTine) <- BlockTine
 */
sealed trait CandidateTine[F[_]]

object CandidateTine {

  implicit def candidateTineContainsSlotId[F[_]]: ContainsSlotId[CandidateTine[F]] = {
    case b: BlockCandidateTine[_]    => b.slotId
    case b: DeepestCommonAncestor[_] => b.slotId
  }
}

/**
 * Represents a Tine for which the local node possesses the referenced Block (by slot ID).  The referenced block
 * has also already been validated against its parent.
 * @param slotId The SlotID of the Block that is known by this node
 * @param parent the parent Tine
 */
case class BlockCandidateTine[F[_]](slotId: SlotId, parent: F[CandidateTine[F]]) extends CandidateTine[F]

object BlockCandidateTine {

  implicit def blockCandidateTineContainsSlotId[F[_]]: ContainsSlotId[BlockCandidateTine[F]] =
    t => t.slotId
}

/**
 * Represents the "trunk" or "root" of all tines in a tine pool.  Early in a blockchain, this Trunk may simply
 * indicate the "genesis" block.  As the blockchain advances, it may not be performant to trace a Tine all the way
 * back to genesis, in which case, the Trunk may just reference some "lookback" limit nearer to the head.
 */
case class DeepestCommonAncestor[F[_]](slotId: SlotId) extends CandidateTine[F]

object DeepestCommonAncestor {

  implicit def deepestCommonAncestorContainsSlotId[F[_]]: ContainsSlotId[DeepestCommonAncestor[F]] =
    t => t.slotId
}

/**
 * A Job is a reverse-singly-linked-list of Block headers that can be traced back to a Block Header _ID_ that is not
 * yet possessed by the node.
 *
 * @example ((PendingJob <- BlockJob) <- BlockJob) <- BlockJob
 */
sealed trait OrphanTine[F[_]]

/**
 * Represents an OrphanTine for which the local node possesses the referenced Block (by slot ID).
 *
 * @param slotId The SlotID of the Block that is known by this node
 * @param parent the parent Job
 */
case class BlockOrphanTine[F[_]](slotId: SlotId, parent: F[OrphanTine[F]]) extends OrphanTine[F]

/**
 * A Job which represents or indicates that a block is not yet possessed by this node, and the node is currently
 * awaiting a response from a peer containing the block.
 *
 * @param blockId the ID of the block that is not yet possessed by this node
 */
case class PendingJob[F[_]](blockId: TypedIdentifier) extends OrphanTine[F]
