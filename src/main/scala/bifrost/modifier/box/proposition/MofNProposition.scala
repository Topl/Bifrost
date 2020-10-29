package bifrost.modifier.box.proposition

import bifrost.crypto.PrivateKey25519
import bifrost.modifier.box.proposition.PublicKey25519Proposition._
import bifrost.utils.serialization.BifrostSerializer
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Curve25519

//noinspection ScalaStyle
case class MofNProposition(m: Int, setOfPubKeyBytes: Set[Array[Byte]]) extends ProofOfKnowledgeProposition[PrivateKey25519] {

  setOfPubKeyBytes.foreach(pubKeyBytes => {
    require(
      pubKeyBytes.length == Curve25519.KeyLength,
      s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${pubKeyBytes.length} found"
    )
  })

  private def bytesWithVersion: Array[Byte] = AddressVersion +: setOfPubKeyBytes.foldLeft(Array[Byte]())(_ ++ _)

  lazy val address: String = Base58.encode(bytesWithVersion ++ calcCheckSum(bytesWithVersion))

  override def toString: String = address

  // TODO: only works for m == 1
  def verify(message: Array[Byte], signature: Array[Byte]): Boolean = {
    setOfPubKeyBytes
      .map(curKeyBytes => Curve25519.verify(signature, message, curKeyBytes))
      .foldLeft(0)((numSuccess, wasSuccessful) => {
        if (wasSuccessful) {
          numSuccess + 1
        } else {
          numSuccess
        }
      }) >= m
  }

  override type M = MofNProposition

  override def serializer: BifrostSerializer[MofNProposition] = MofNPropositionSerializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: MofNProposition => p.m == m && p.setOfPubKeyBytes == setOfPubKeyBytes
    case _                  => false
  }

  override def hashCode(): Int = (BigInt(Blake2b256(serializer.toBytes(this))) % Int.MaxValue).toInt
}
