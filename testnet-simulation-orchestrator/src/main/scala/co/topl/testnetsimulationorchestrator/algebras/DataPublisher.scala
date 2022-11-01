package co.topl.testnetsimulationorchestrator.algebras

import co.topl.testnetsimulationorchestrator.models._

trait DataPublisher[F[_], G[_]] {
  def publishAdoptions(results:    G[AdoptionDatum], node: String): F[Unit]
  def publishBlocks(results:       G[BlockDatum]): F[Unit]
  def publishTransactions(results: G[TransactionDatum]): F[Unit]
}
