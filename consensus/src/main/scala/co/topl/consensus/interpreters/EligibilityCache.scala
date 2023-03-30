package co.topl.consensus.interpreters

import cats.Applicative
import cats.Monad
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import co.topl.consensus.algebras.EligibilityCacheAlgebra
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.models.Bytes
import co.topl.models.Slot
import com.google.protobuf.ByteString

import scala.collection.immutable.SortedSet

object EligibilityCache {

  /**
   * Constructs an EligibilityCacheAlgebra that is backed by a Ref containing a "State" object.  The State maintains a
   * set of "recently" claimed eligibilities.  "Recently" is defined in terms of claimed slot number, not in terms of time.
   *
   * This mechanism only prevents eligibility re-use near the current global slot.  An adversary may still attempt to feed
   * old eligibilities into the system which would make it past this check, but the adversary would need to overcome
   * the chain growth of the honest chain for the attack to work anyway.
   *
   * @param maximumLength The maximum number of entries allowed in the cache.  Older entries (based on slot) are evicted
   *                      as new entries are received.
   */
  def make[F[_]: Async](maximumLength: Int): Resource[F, EligibilityCacheAlgebra[F]] =
    for {
      ref <- Ref.of(State(maximumLength)).toResource
    } yield new EligibilityCacheAlgebra[F] {

      def tryInclude(vrfVK: Bytes, slot: Slot): F[Boolean] =
        ref.modify(_.tryInclude(vrfVK, slot))
    }

  def repopulate[F[_]: Monad](
    underlying:    EligibilityCacheAlgebra[F],
    maximumLength: Int,
    canonicalHead: BlockHeader,
    fetchHeader:   BlockId => F[BlockHeader]
  ): F[Unit] =
    // Exclude the genesis eligibility
    if (canonicalHead.height <= 1)
      Applicative[F].unit
    else
      canonicalHead
        .iterateUntilM(header =>
          for {
            _            <- underlying.tryInclude(header.eligibilityCertificate.vrfVK, header.slot)
            parentHeader <- fetchHeader(header.parentHeaderId)
          } yield parentHeader
        )(header => header.height <= 1 || header.height > (canonicalHead.height - maximumLength))
        .void

  /**
   * The internal state of the cache
   * @param maxLength The maximum number of entries allowed in the cache.  If the cache is full and a new
   *                  entry is provided, the oldest entry (by slot) will be removed
   * @param entries A set of entries in the cache, sorted by Slot
   */
  private case class State(maxLength: Int, entries: SortedSet[(Slot, Bytes)] = SortedSet.empty) {

    /**
     * Attempt to include the entry.  If the entry already existed, the state remains unchanged, and "false" is returned.
     * Otherwise, a new state and "true" is returned
     */
    def tryInclude(vrfVK: Bytes, slot: Slot): (State, Boolean) = {
      // Note: An entry may already exist in the cache at the provided vrfVK under a different slot.  A future
      // optimization may be to take the vrfVK instance from the old entry and use it in the new entry as well.
      // This would avoid a memory penalty of duplicating the same bytes in memory, but would come at the expense of
      // CPU time to perform the initial search for an existing entry.
      val newEntry = (slot, vrfVK)
      if (entries.contains(newEntry)) this         -> false
      else copy(entries = entries + newEntry).trim -> true
    }

    /**
     * Discard old entries to keep the entry-set within the [[maxLength]] bound
     */
    private def trim: State =
      copy(entries = entries.takeRight(maxLength))
  }

  /**
   * An implicit ordering based on slot, with a tie-breaker on the VRF VK
   */
  implicit private val orderingEntries: Ordering[(Slot, Bytes)] = new Ordering[(Slot, Bytes)] {

    implicit private val bytesOrdering: Ordering[Bytes] =
      Ordering.comparatorToOrdering(ByteString.unsignedLexicographicalComparator())

    def compare(x: (Slot, Bytes), y: (Slot, Bytes)): Int =
      if (x._1 > y._1) 1
      else if (y._1 > x._1) -1
      else bytesOrdering.compare(x._2, y._2)
  }
}
