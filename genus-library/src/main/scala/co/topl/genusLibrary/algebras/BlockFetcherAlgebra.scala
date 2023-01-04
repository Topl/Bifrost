package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.HeightData

/**
 * Fetcher of blocks on the chain.
 * @tparam F the effect-ful context to retrieve the value in
 */
trait BlockFetcherAlgebra[F[_]] {

  /**
   * Look-up a block on the chain with a given height
   * @param height The height to lookup
   * @return the full block
   */
  // TODO: TSDK-216 | Create protobuf files
  def fetch(height: Long): F[Either[Failure, HeightData]]

}
