package bifrost.crypto

import bifrost.modifier.box.proposition.{Proposition, PublicKey25519Proposition}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.util.Try

/**
  * @param signature 25519 signature
  */
case class Signature25519(signature: Array[Byte]) extends ProofOfKnowledge[PrivateKey25519, PublicKey25519Proposition] {
  require(signature.isEmpty || signature.length == Curve25519.SignatureLength,
    s"${signature.length} != ${Curve25519.SignatureLength}")

  override def isValid(proposition: Proposition, message: Array[Byte]): Boolean =
    Curve25519.verify(signature, message, proposition.bytes)

  override type M = Signature25519

  override def serializer: BifrostSerializer[Signature25519] = Signature25519Serializer

  override def toString: String = s"Signature25519(${Base58.encode(signature)})"
}

object Signature25519Serializer extends BifrostSerializer[Signature25519] {
  override def toBytes(obj: Signature25519): Array[Byte] = obj.signature

  override def parseBytes(bytes: Array[Byte]): Try[Signature25519] = Try(Signature25519(bytes))

  override def serialize(obj: Signature25519, w: Writer): Unit = ???

  override def parse(r: Reader): Signature25519 = ???
}

object Signature25519 {
  lazy val SignatureSize: Int = Curve25519.SignatureLength
}
