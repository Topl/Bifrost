package bifrost.transaction.box.proposition

import com.google.common.primitives.{Bytes, Ints}
import bifrost.serialization.Serializer
import bifrost.transaction.box.proposition.PublicKey25519Proposition._
import bifrost.transaction.box.proposition._
import bifrost.transaction.state.PrivateKey25519
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.Curve25519

import scala.util.Try

//noinspection ScalaStyle
case class MofNProposition(m: Int, setOfPubKeyBytes: Set[Array[Byte]])
  extends ProofOfKnowledgeProposition[PrivateKey25519] {

  setOfPubKeyBytes.foreach(pubKeyBytes => {
    require(pubKeyBytes.length == Curve25519.KeyLength,
            s"Incorrect pubKey length, ${Curve25519.KeyLength} expected, ${pubKeyBytes.length} found")
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

  override def serializer: Serializer[MofNProposition] = MofNPropositionSerializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case p: MofNProposition => p.m == m && p.setOfPubKeyBytes == setOfPubKeyBytes
    case _ => false
  }

  override def hashCode(): Int = (BigInt(Blake2b256(serializer.toBytes(this))) % Int.MaxValue).toInt

}

object MofNPropositionSerializer extends Serializer[MofNProposition] {
  override def toBytes(obj: MofNProposition): Array[Byte] = Bytes.concat(
    Ints.toByteArray(obj.m),
    Ints.toByteArray(obj.setOfPubKeyBytes.size),
    obj.setOfPubKeyBytes.toList.sortBy(Base58.encode).foldLeft(Array[Byte]())((a: Array[Byte],
                                                                               b: Array[Byte]) => a ++ b)
  )

  override def parseBytes(bytes: Array[Byte]): Try[MofNProposition] = Try {

    val m = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val n = Ints.fromByteArray(bytes.slice(Ints.BYTES, 2 * Ints.BYTES))

    val setPubKeys = (0 until n).map { i =>
      bytes.slice(2 * Ints.BYTES + i * Constants25519.PubKeyLength,
                  2 * Ints.BYTES + (i + 1) * Constants25519.PubKeyLength)
    }.foldLeft(Set[Array[Byte]]())((set: Set[Array[Byte]], pubKey: Array[Byte]) => set + pubKey)

    MofNProposition(m, setPubKeys)
  }
}