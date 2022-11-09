package co.topl.testnetsimulationorchestrator.algebras

import co.topl.testnetsimulationorchestrator.models._

/**
 * Exports result of the testnet to an external destination
 * @tparam F an effect wrapper
 * @tparam G a collection/stream wrapper
 */
trait DataPublisher[F[_], G[_]] {

  /**
   * Publish data about a specific node's block adoptions
   */
  def publishAdoptions(results: G[AdoptionDatum], node: String): F[Unit]

  /**
   * Publish data about all blocks discovered in the simulation
   */
  def publishBlocks(results: G[BlockDatum]): F[Unit]

  /**
   * Publish data about all transactions discovered in the simulation
   */
  def publishTransactions(results: G[TransactionDatum]): F[Unit]
}
