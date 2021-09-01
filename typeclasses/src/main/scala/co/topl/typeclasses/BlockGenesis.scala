package co.topl.typeclasses

import cats.Eval
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Lengths, Sized}
import co.topl.typeclasses.ContainsTransactions.Instances._
import co.topl.typeclasses.ContainsTransactions.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

object BlockGenesis {
  val ParentId: TypedIdentifier = TypedBytes(IdentifierTypes.Block.HeaderV2, Bytes(Array.fill[Byte](32)(0)))

  val vrfCertificate: VrfCertificate = VrfCertificate(
    PublicKeys.Vrf(
      PublicKeys.Ed25519(Sized.strict[Bytes, PublicKeys.Ed25519.Length](Bytes(Array.fill(32)(0: Byte))).toOption.get)
    ),
    Proofs.Consensus.Nonce(Sized.strict[Bytes, Lengths.`80`.type](Bytes(Array.fill(80)(0: Byte))).toOption.get),
    Proofs.Consensus.VrfTest(Sized.strict[Bytes, Lengths.`80`.type](Bytes(Array.fill(80)(0: Byte))).toOption.get)
  )

  val kesCertificate: KesCertificate =
    KesCertificate(
      PublicKeys.Kes(
        Sized.strict[Bytes, PublicKeys.Kes.Length](Bytes(Array.fill(32)(0: Byte))).toOption.get,
        slot = 0
      ),
      Proofs.Consensus.KesCertificate(
        Sized.strict[Bytes, Lengths.`64`.type](Bytes(Array.fill(64)(0: Byte))).toOption.get
      ),
      Proofs.Consensus.MMM(Sized.strict[Bytes, Lengths.`1440`.type](Bytes(Array.fill(1440)(0: Byte))).toOption.get),
      slotOffset = 0
    )

  def apply(transactions: Seq[Transaction]): Eval[BlockV2] = Eval.later {
    val address = TaktikosAddress(
      Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(0))).toOption.get,
      Sized.strict[Bytes, Lengths.`32`.type](Bytes(Array.fill[Byte](32)(0))).toOption.get,
      Sized.strict[Bytes, Lengths.`64`.type](Bytes(Array.fill[Byte](64)(0))).toOption.get
    )
    val header =
      BlockHeaderV2(
        parentHeaderId = ParentId,
        txRoot = transactions.merkleTree,
        bloomFilter = transactions.bloomFilter,
        timestamp = 0L,
        height = 1,
        slot = 0,
        vrfCertificate = vrfCertificate,
        kesCertificate = kesCertificate,
        thresholdEvidence = Sized
          .strict[TypedBytes, Lengths.`33`.type](
            TypedBytes(IdentifierTypes.RatioEvidence, Bytes(Array.fill[Byte](32)(0)))
          )
          .toOption
          .get,
        metadata = None,
        address = address
      )
    val body =
      BlockBodyV2(
        transactions = transactions,
        headerId = header.id
      )
    BlockV2(header, body)
  }
}
