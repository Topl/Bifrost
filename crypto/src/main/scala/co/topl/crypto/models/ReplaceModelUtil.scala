package co.topl.crypto.models

import co.topl.models.Proofs.Knowledge.KesProduct
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.{Bytes, Proofs, VerificationKeys}
import co.topl.models.utility.Sized
import co.topl.models.utility._

/**
 * Delete this file when replacement job is done
 * Used on Staker initialzer to create Box.Values.Registrations.Operator
 * Once Operator is converted to new model, delete this util
 */
object ReplaceModelUtil {

  def signatureKesProduct(signatureKesProduct: co.topl.crypto.models.SignatureKesProduct): Proofs.Knowledge.KesProduct =
    KesProduct(
      signatureKesSum(signatureKesProduct.superSignature),
      signatureKesSum(signatureKesProduct.subSignature),
      Sized.strictUnsafe[Bytes, KesProduct.DigestLength](signatureKesProduct.subRoot)
    )

  def signatureKesSum(signatureKesSum: co.topl.crypto.models.SignatureKesSum): Proofs.Knowledge.KesSum =
    Proofs.Knowledge.KesSum(
      verificationKeysEd25519(signatureKesSum.verificationKey),
      signatureEd25519(signatureKesSum.signature),
      signatureKesSum.witness.toVector.map(Sized.strictUnsafe[Bytes, Proofs.Knowledge.KesSum.DigestLength](_))
    )

  def signatureEd25519(signature: co.topl.crypto.models.SignatureEd25519): Proofs.Knowledge.Ed25519 =
    Proofs.Knowledge.Ed25519(
      Sized.strictUnsafe[Bytes, Proofs.Knowledge.Ed25519.Length](signature.value)
    )

  def verificationKeysEd25519(vk: co.topl.crypto.models.VerificationKeyEd25519): VerificationKeys.Ed25519 =
    VerificationKeys.Ed25519(
      Sized.strictUnsafe[Bytes, VerificationKeys.Ed25519.Length](vk.value)
    )

}
