package co.topl.consensus.algebras

import co.topl.consensus.models.{ChainSelectionOutcome, SlotData}

trait ChainSelectionAlgebra[F[_]] {

  /**
   * Compare the best block of two separate branches
   * @param xHead The best block of branch X (generally the local branch)
   * @param yHead The best block of branch Y
   * @param commonAncestor The shared ancestor block between X and Y
   * @param fetchXAtHeight A function to fetch the block-by-height from X
   * @param fetchYAtHeight A function to fetch the block-by-height from Y
   * @return A ChainSelectionOutcome
   */
  def compare(
    xHead:          SlotData,
    yHead:          SlotData,
    commonAncestor: SlotData,
    fetchXAtHeight: Long => F[Option[SlotData]],
    fetchYAtHeight: Long => F[Option[SlotData]]
  ): F[ChainSelectionOutcome]
}
