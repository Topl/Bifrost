package co.topl.consensus.interpreters

import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.algebras.EligibilityCacheAlgebra
import co.topl.consensus.models._
import co.topl.models._
import co.topl.typeclasses.implicits._
import com.google.protobuf.ByteString

import scala.collection.immutable.SortedMap

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
      ref <- Ref.of(EligibilityState(maximumLength)).toResource
    } yield new EligibilityCacheAlgebra[F] {

      override def tryInclude(blockId: BlockId, vrfVK: Bytes, slot: Slot): F[Boolean] =
        ref.modify(_.tryInclude(blockId, vrfVK, slot))
    }

  /**
   * When a node is launched, eligibilities from adopted blocks should be added to the cache.
   *
   * @param underlying    The underlying cache to populate
   * @param maximumLength The maximum number of entries in the underlying cache
   * @param canonicalHead The current head of the chain
   * @param fetchHeader   Header lookup function (to traverse ancestors)
   */
  def repopulate[F[_]: Async](
    underlying:    EligibilityCacheAlgebra[F],
    maximumLength: Int,
    canonicalHead: BlockHeader,
    fetchHeader:   BlockId => F[BlockHeader]
  ): F[Unit] = {
    import fs2._
    // A stream of block headers from the canonical head back to the genesis block
    // The canonical head is not included in this stream.
    val ancestors =
      Stream.unfoldEval(canonicalHead)(header =>
        if (header.height <= 1) none[(BlockHeader, BlockHeader)].pure[F]
        else fetchHeader(header.parentHeaderId).map(parentH => (parentH, parentH).some)
      )
    (Stream(canonicalHead) ++ ancestors)
      .take(maximumLength)
      .evalMap(header => underlying.tryInclude(header.id, header.eligibilityCertificate.vrfVK, header.slot))
      .compile
      .drain
  }

  /**
   * The internal state of the cache
   * @param maxLength The maximum number of entries allowed in the cache.  If the cache is full and a new
   *                  entry is provided, the oldest entry (by slot) will be removed
   * @param entries A sorted map of entries in the cache, sorted by Slot
   */
  private case class EligibilityState(maxLength: Int, entries: SortedMap[(Slot, Bytes), BlockId] = SortedMap.empty) {

    /**
     * Attempt to include the entry.  If the entry already existed, the state remains unchanged, and "false" is returned.
     * Otherwise, a new state and "true" is returned
     */
    def tryInclude(blockId: BlockId, vrfVK: Bytes, slot: Slot): (EligibilityState, Boolean) = {
      // Note: An entry may already exist in the cache at the provided vrfVK under a different slot.  A future
      // optimization may be to take the vrfVK instance from the old entry and use it in the new entry as well.
      // This would avoid a memory penalty of duplicating the same bytes in memory, but would come at the expense of
      // CPU time to perform the initial search for an existing entry.
      val newEntry = (slot, vrfVK)
      entries.get(newEntry) match {
        // An entry already exists, so check if the entry corresponds to the provided block
        case Some(entry) => this                                                 -> (entry === blockId)
        case _           => copy(entries = entries + (newEntry -> blockId)).trim -> true
      }
    }

    /**
     * Discard old entries to keep the entry-set within the [[maxLength]] bound
     */
    private def trim: EligibilityState =
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
