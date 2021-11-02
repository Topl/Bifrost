package co.topl.typeclasses

import cats.Eval
import co.topl.models.VerificationKeys.VrfEd25519
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Length, Lengths, Sized}
import co.topl.typeclasses.ContainsTransactions.Instances._
import co.topl.typeclasses.ContainsTransactions.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

object BlockGenesis {

  def zeroBytes[L <: Length](implicit l: L): Sized.Strict[Bytes, L] =
    Sized.strictUnsafe[Bytes, L](Bytes(Array.fill(l.value)(0: Byte)))

  val ParentId: TypedIdentifier = TypedBytes(IdentifierTypes.Block.HeaderV2, Bytes(Array.fill[Byte](32)(0)))
  val ParentSlot: Slot = -1

  val vrfCertificate: EligibilityCertificate = EligibilityCertificate(
    Proofs.Signature.VrfEd25519(zeroBytes(Lengths.`80`)),
    Proofs.Signature.VrfEd25519(zeroBytes(Lengths.`80`)),
    VerificationKeys.VrfEd25519(VerificationKeys.Ed25519(zeroBytes[VrfEd25519.Length]).bytes),
    thresholdEvidence = Sized.strictUnsafe(TypedBytes(IdentifierTypes.RatioEvidence, Bytes(Array.fill[Byte](32)(0)))),
    eta = zeroBytes
  )

//  val kesCertificate: OperationalCertificate =
//    OperationalCertificate(
//      opSig = Proofs.Signature.HdKes(
//        i = 0,
//        vkI = VerificationKeys.Ed25519(zeroBytes),
//        ecSignature = Proofs.Signature.Ed25519(zeroBytes),
//        sigSumJ = Proofs.Signature.SumProduct(
//          ecSignature = Proofs.Signature.Ed25519(zeroBytes),
//          vkK = VerificationKeys.Ed25519(zeroBytes),
//          index = 0,
//          witness = Nil
//        ),
//        sigSumK = Proofs.Signature.SumProduct(
//          ecSignature = Proofs.Signature.Ed25519(zeroBytes),
//          vkK = VerificationKeys.Ed25519(zeroBytes),
//          index = 0,
//          witness = Nil
//        )
//      ),
//      xvkM = VerificationKeys.ExtendedEd25519(VerificationKeys.Ed25519(zeroBytes), zeroBytes),
//      slotR = 0
//    )
  val kesCertificate: OperationalCertificate = OperationalCertificate(
    Proofs.Signature.Ed25519(Sized.strictUnsafe(Bytes(Array.fill(64)(0: Byte))))
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
        eligibilityCertificate = vrfCertificate,
        operationalCertificate = kesCertificate,
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
