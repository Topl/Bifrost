package co.topl.typeclasses

import cats.Eval
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Length, Lengths, Sized}
import co.topl.typeclasses.ContainsTransactions.Instances._
import co.topl.typeclasses.ContainsTransactions.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

object BlockGenesis {

  private def zeroBytes[L <: Length](implicit l: L): Sized.Strict[Bytes, L] =
    Sized.strictUnsafe[Bytes, L](Bytes(Array.fill(l.value)(0: Byte)))

  val ParentId: TypedIdentifier = TypedBytes(IdentifierTypes.Block.HeaderV2, Bytes(Array.fill[Byte](32)(0)))
  val ParentSlot: Slot = -1

  val vrfCertificate: Vrf.Certificate = Vrf.Certificate(
    VerificationKeys.Vrf(VerificationKeys.Ed25519(zeroBytes[VerificationKeys.Ed25519.Length])),
    Proofs.Consensus.Nonce(zeroBytes(Lengths.`80`)),
    Proofs.Consensus.VrfTest(zeroBytes(Lengths.`80`))
  )

  val kesCertificate: OperationalCertificate =
    KesCertificate(
      VerificationKeys.Kes(
        zeroBytes[VerificationKeys.HdKes.Length],
        offset = 0
      ),
      Proofs.Consensus.KesCertificate(
        zeroBytes[Proofs.Consensus.KesCertificate.SignatureLength],
        zeroBytes[Proofs.Consensus.KesCertificate.ExtendedPublicKeyLength],
        zeroBytes[Proofs.Consensus.KesCertificate.ChainCodeLength]
      ),
      Proofs.Consensus.MMM(
        Bytes(Array.fill(704)(0)),
        Bytes(Array.fill(704)(0)),
        Bytes(Array.fill(32)(0)),
        0,
        Bytes(Array.fill(32)(0))
      )
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
        parentSlot = ParentSlot,
        txRoot = transactions.merkleTree,
        bloomFilter = transactions.bloomFilter,
        timestamp = 0L,
        height = 1,
        slot = 0,
        eligibibilityCertificate = vrfCertificate,
        operationalCertificate = kesCertificate,
        thresholdEvidence =
          Sized.strictUnsafe(TypedBytes(IdentifierTypes.RatioEvidence, Bytes(Array.fill[Byte](32)(0)))),
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
