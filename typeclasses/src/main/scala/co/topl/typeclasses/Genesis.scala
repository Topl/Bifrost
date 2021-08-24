package co.topl.typeclasses

import co.topl.models.HasLength.implicits._
import co.topl.models.Lengths._
import co.topl.models._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._
import simulacrum.typeclass

/**
 * Satisfies that a genesis/first/initial instance of T can be created
 */
@typeclass trait Genesis[T] {
  def create(): T
}

object BlockGenesis {
  val ParentId: TypedIdentifier = TypedBytes(IdentifierTypes.Block.HeaderV2, Bytes(Array.fill[Byte](32)(0)))

  val vrfCertificate: VrfCertificate = VrfCertificate(
    PublicKeys.Ed25519(Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill(32)(0: Byte))).toOption.get),
    Sized.strict[Bytes, Lengths.`64`.type](Bytes(Array.fill(64)(0: Byte))).toOption.get,
    Sized.strict[Bytes, Lengths.`80`.type](Bytes(Array.fill(80)(0: Byte))).toOption.get
  )
  val KesCertificate: Bytes = Bytes(Array.fill[Byte](32)(0))

  def apply(transactions: Seq[Transaction]): Genesis[BlockV2] = { () =>
    val body =
      BlockBodyV2(
        transactions = transactions,
        parentHeaderId = ParentId
      )
    BlockV2(
      BlockHeaderV2(
        parentHeaderId = ParentId,
        blockBodyId = body.id,
        timestamp = 0L,
        height = 1,
        slot = 0,
        vrfCertificate = vrfCertificate,
        kesCertificate = KesCertificate
      ),
      body
    )
  }
}
