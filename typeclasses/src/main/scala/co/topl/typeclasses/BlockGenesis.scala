package co.topl.typeclasses

import cats.Eval
import co.topl.models._
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Length, Lengths, Sized}
import co.topl.typeclasses.ContainsTransactions.Instances._
import co.topl.typeclasses.ContainsTransactions.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

object BlockGenesis {

  private def zeroBytes[L <: Length](implicit l: L): Sized.Strict[Bytes, L] =
    Sized.strict[Bytes, L](Bytes(Array.fill(l.value)(0: Byte))).toOption.get

  val ParentId: TypedIdentifier = TypedBytes(IdentifierTypes.Block.HeaderV2, Bytes(Array.fill[Byte](32)(0)))

  val vrfCertificate: VrfCertificate = VrfCertificate(
    PublicKeys.Vrf(PublicKeys.Ed25519(zeroBytes[PublicKeys.Ed25519.Length])),
    Proofs.Consensus.Nonce(zeroBytes(Lengths.`80`)),
    Proofs.Consensus.VrfTest(zeroBytes(Lengths.`80`))
  )

  val kesCertificate: KesCertificate =
    KesCertificate(
      PublicKeys.Kes(
        zeroBytes[PublicKeys.Kes.Length],
        slot = 0
      ),
      Proofs.Consensus.KesCertificate(
        zeroBytes(Lengths.`64`)
      ),
      Proofs.Consensus.MMM(zeroBytes(Lengths.`1440`)),
      slotOffset = 0
    )

  def apply(transactions: Seq[Transaction]): Eval[BlockV2] = Eval.later {
    val address = TaktikosAddress(
      zeroBytes(Lengths.`32`),
      zeroBytes(Lengths.`32`),
      zeroBytes(Lengths.`64`)
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
