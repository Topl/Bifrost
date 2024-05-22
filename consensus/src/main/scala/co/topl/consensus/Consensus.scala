package co.topl.consensus

import co.topl.consensus.algebras._
import co.topl.consensus.models.SlotData

trait Consensus[F[_]] {
  def headerValidation: BlockHeaderValidationAlgebra[F]
  def headerToBodyValidation: BlockHeaderToBodyValidationAlgebra[F]
  def chainSelection: ChainSelectionAlgebra[F, SlotData]
  def consensusValidationState: ConsensusValidationStateAlgebra[F]
  def etaCalculation: EtaCalculationAlgebra[F]
  def leaderElection: LeaderElectionValidationAlgebra[F]
  def localChain: LocalChainAlgebra[F]
}

case class ConsensusImpl[F[_]](
  headerValidation:         BlockHeaderValidationAlgebra[F],
  headerToBodyValidation:   BlockHeaderToBodyValidationAlgebra[F],
  chainSelection:           ChainSelectionAlgebra[F, SlotData],
  consensusValidationState: ConsensusValidationStateAlgebra[F],
  etaCalculation:           EtaCalculationAlgebra[F],
  leaderElection:           LeaderElectionValidationAlgebra[F],
  localChain:               LocalChainAlgebra[F]
) extends Consensus[F]
