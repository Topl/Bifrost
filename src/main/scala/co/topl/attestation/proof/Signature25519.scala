package co.topl.attestation.proof

import co.topl.attestation.evidence.Evidence
import co.topl.attestation.proposition.{ Proposition, PublicKey25519Proposition }
import co.topl.attestation.proof.serialization.Signature25519Serializer
import co.topl.attestation.secrets.PrivateKey25519
import co.topl.utils.serialization.BifrostSerializer
import io.circe.syntax.EncoderOps
import io.circe.{ Decoder, Encoder, KeyDecoder, KeyEncoder }
import scorex.crypto.signatures.{ Curve25519, PublicKey, Signature }
import scorex.util.encode.Base58

import scala.util.{ Failure, Success, Try }

/**
  * @param signature 25519 signature
  */
case class Signature25519 (signature: Signature) extends ProofOfKnowledge[PrivateKey25519, Evidence] {
  require(signature.isEmpty || signature.length == Curve25519.SignatureLength,
    s"${signature.length} != ${Curve25519.SignatureLength}")

  override type M = Signature25519

  override def serializer: BifrostSerializer[Signature25519] = Signature25519Serializer

  override def isValid (proposition: Proposition, message: Array[Byte]): Boolean =
    Curve25519.verify(signature, message, PublicKey @@ proposition.bytes)

  override def toString: String = s"Signature25519(${Base58.encode(signature)})"
}




object Signature25519 {
  lazy val SignatureSize: Int = Curve25519.SignatureLength

  def apply (encodedSig: String): Signature25519 = {
    if (encodedSig.isEmpty) Signature25519(Signature @@ Array.emptyByteArray)
    else {
      Base58.decode(encodedSig) match {
        case Success(sig) => new Signature25519(Signature @@ sig)
        case Failure(ex)  => throw ex
      }
    }
  }

  /** Returns a signature filled with 1's for use in genesis signatures */
  def genesis (): Signature25519 = {
    Signature25519(Signature @@ Array.fill(Signature25519.SignatureSize)(1: Byte))
  }

  /** Helper function to create empty signatures */
  def empty (): Signature25519 = Signature25519(Signature @@ Array.emptyByteArray)


  // see circe documentation for custom encoder / decoders
  // https://circe.github.io/circe/codecs/custom-codecs.html
  implicit val jsonEncoder: Encoder[Signature25519] =
    (sig: Signature25519) => sig.toString.asJson

  implicit val jsonDecoder: Decoder[Signature25519] =
    Decoder.decodeString.emapTry { sig => Try(Signature25519(sig)) }

  implicit val jsonKeyEncoder: KeyEncoder[Signature25519] =
    ( sig: Signature25519 ) => sig.toString

  implicit val jsonKeyDecoder: KeyDecoder[Signature25519] =
    ( sig: String ) => Some(Signature25519(sig))
}
