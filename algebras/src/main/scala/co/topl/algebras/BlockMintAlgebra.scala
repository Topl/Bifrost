package co.topl.algebras

import co.topl.models.{BlockHeaderV2, BlockV2, Timestamp, Transaction}

trait BlockMintAlgebra[F[_]] {
  def mint(parent: BlockHeaderV2, transactions: Seq[Transaction]): F[Timestamp => BlockV2.Unsigned]
}
