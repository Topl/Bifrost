package co.topl.crypto.models

import co.topl.models.Proofs.Knowledge.KesProduct
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.{Bytes, Proofs, VerificationKeys}
import co.topl.models.utility.Sized

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
      Sized.strictUnsafe[Bytes, KesProduct.DigestLength](Bytes(signatureKesProduct.subRoot))
    )

  def signatureKesSum(signatureKesSum: co.topl.crypto.models.SignatureKesSum): Proofs.Knowledge.KesSum =
    Proofs.Knowledge.KesSum(
      VerificationKeys.Ed25519(
        Sized.strictUnsafe[Bytes, VerificationKeys.Ed25519.Length](Bytes(signatureKesSum.verificationKey))
      ),
      Proofs.Knowledge.Ed25519(
        Sized.strictUnsafe[Bytes, Proofs.Knowledge.Ed25519.Length](Bytes(signatureKesSum.signature))
      ),
      signatureKesSum.witness.toVector.map(bytes =>
        Sized.strictUnsafe[Bytes, Proofs.Knowledge.KesSum.DigestLength](Bytes(bytes))
      )
    )

}
