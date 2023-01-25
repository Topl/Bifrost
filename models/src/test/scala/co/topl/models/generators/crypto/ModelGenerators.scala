package co.topl.models.generators.crypto

import co.topl.crypto.models._
import co.topl.models.generators.common.ModelGenerators._
import co.topl.models.utility.Lengths
import org.scalacheck.Gen

/**
 * Model Generators related to: https://github.com/Topl/protobuf-specs/blob/main/crypto
 */
trait ModelGenerators {

  def verificationKeyEd25519Gen: Gen[VerificationKeyEd25519] =
    genSizedStrictByteString[Lengths.`32`.type]().map(s => VerificationKeyEd25519.of(s.data))

  def secretKeyEd25519Gen: Gen[SecretKeyEd25519] =
    genSizedStrictByteString[Lengths.`32`.type]().map(s => SecretKeyEd25519.of(s.data))

  def signatureEd25519Gen: Gen[SignatureEd25519] =
    genSizedStrictByteString[Lengths.`64`.type]().map(s => SignatureEd25519.of(s.data))

}
object ModelGenerators extends ModelGenerators
