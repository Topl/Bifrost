package co.topl.typeclasses

import cats.Eval
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.models.VerificationKeys.VrfEd25519
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.{Length, Lengths, Sized}
import co.topl.typeclasses.implicits._

import java.nio.charset.StandardCharsets

object BlockGenesis {

  def zeroBytes[L <: Length](implicit l: L): Sized.Strict[Bytes, L] =
    Sized.strictUnsafe[Bytes, L](Bytes(Array.fill(l.value)(0: Byte)))

  val ParentId: TypedIdentifier = TypedBytes(IdentifierTypes.Block.HeaderV2, Bytes(Array.fill[Byte](32)(0)))
  val ParentSlot: Slot = -1

  def vrfCertificate(eta: Eta): EligibilityCertificate = EligibilityCertificate(
    Proofs.Knowledge.VrfEd25519(zeroBytes(Lengths.`80`)),
    VerificationKeys.VrfEd25519(VerificationKeys.Ed25519(zeroBytes[VrfEd25519.Length]).bytes),
    thresholdEvidence = Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0))),
    eta = eta
  )

  val kesCertificate: OperationalCertificate = OperationalCertificate(
    VerificationKeys.KesProduct(zeroBytes(Lengths.`32`), 0),
    Proofs.Knowledge.KesProduct(
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
        Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`)),
        Vector.empty
      ),
      Proofs.Knowledge.KesSum(
        VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
        Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`)),
        Vector.empty
      ),
      zeroBytes(Lengths.`32`)
    ),
    VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
    Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`))
  )

  def apply(transactions: Seq[Transaction]): Eval[BlockV2] = Eval.later {
    val address = TaktikosAddress(
      zeroBytes(Lengths.`32`),
      VerificationKeys.Ed25519(zeroBytes(Lengths.`32`)),
      Proofs.Knowledge.Ed25519(zeroBytes(Lengths.`64`))
    )

    // TODO: Read "genesis-eta-plaintext" from application.conf, and then hash that value and/or Magic Bytes
    val eta: Eta =
      new Blake2b256().hash(
        Bytes("genesis".getBytes(StandardCharsets.UTF_8)) +:
        transactions.map(_.immutableBytes): _*
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
        eligibilityCertificate = vrfCertificate(eta),
        operationalCertificate = kesCertificate,
        metadata = None,
        address = address
      )
    BlockV2(header, transactions.toList.map(_.id))
  }
}
