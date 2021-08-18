package co.topl.typeclasses

import co.topl.models._
import simulacrum.typeclass

/**
 * Satisfies that a genesis/first/initial instance of T can be created
 */
@typeclass trait Genesis[T] {
  def create(): T
}

object BlockGenesis {
  val ParentId: TypedIdentifier = TypedBytes(IdentifierTypes.Block.V2, Bytes(Array.fill[Byte](32)(0)))
  val VrfCertificate: Bytes = Bytes(Array.fill[Byte](32)(0))
  val KesCertificate: Bytes = Bytes(Array.fill[Byte](32)(0))

  def apply(transactions: Seq[Transaction]): Genesis[Block] =
    () =>
      BlockV2(
        parentId = ParentId,
        timestamp = 0L,
        height = 1,
        transactions = transactions,
        slot = 0,
        vrfCertificate = VrfCertificate,
        kesCertificate = KesCertificate
      )
}
