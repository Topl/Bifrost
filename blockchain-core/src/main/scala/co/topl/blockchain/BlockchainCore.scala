package co.topl.blockchain

import co.topl.algebras.{ClockAlgebra, ProtocolConfigurationAlgebra}
import co.topl.blockchain.algebras.EpochDataAlgebra
import co.topl.consensus.Consensus
import co.topl.consensus.models.BlockId
import co.topl.eventtree.ParentChildTree
import co.topl.ledger.Ledger
import fs2.Stream

trait BlockchainCore[F[_]] {
  def clock: ClockAlgebra[F]
  def dataStores: DataStores[F]
  def cryptoResources: CryptoResources[F]
  def blockIdTree: ParentChildTree[F, BlockId]
  def consensus: Consensus[F]
  def ledger: Ledger[F]
  def validators: Validators[F]
  def epochData: EpochDataAlgebra[F]
  def protocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]]
}

case class BlockchainCoreImpl[F[_]](
  clock:                 ClockAlgebra[F],
  dataStores:            DataStores[F],
  cryptoResources:       CryptoResources[F],
  blockIdTree:           ParentChildTree[F, BlockId],
  consensus:             Consensus[F],
  ledger:                Ledger[F],
  validators:            Validators[F],
  epochData:             EpochDataAlgebra[F],
  protocolConfiguration: ProtocolConfigurationAlgebra[F, Stream[F, *]]
) extends BlockchainCore[F]
