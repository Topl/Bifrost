package co.topl.algebras

import co.topl.models.{BlockHeaderV2, BlockV2, Timestamp, Transaction, Vrf}

trait BlockMintAlgebra[F[_]] {
  def mint(parent: BlockHeaderV2, transactions: Seq[Transaction], vrfHit: Vrf.Hit): F[Timestamp => BlockV2.Unsigned]
}
