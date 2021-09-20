package co.topl.attestation.keyManagement.stakingKeys

import co.topl.crypto.kes
import co.topl.attestation.keyManagement.derivedKeys.{
  DerivedKeyIndex,
  ExtendedPrivateKeyEd25519,
  ExtendedPublicKeyEd25519,
  SoftIndex
}
import co.topl.attestation.keyManagement.mnemonic
import co.topl.crypto.kes.keys.SymmetricKey
import co.topl.crypto.hash.blake2b256
import com.google.common.primitives.{Bytes, Ints, Longs}
import scodec.bits.ByteOrdering
import co.topl.utils.SizedBytes
import co.topl.utils.SizedBytes.Types.ByteVector32
import co.topl.utils.SizedBytes.implicits._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class HdKesScheme(
  registrationSlot:    Long,
  rootVerificationKey: ExtendedPublicKeyEd25519,
  privateKeySet:       mutable.Map[SoftIndex, ExtendedPrivateKeyEd25519]
) {

  import HdKesScheme.serializer

  def deriveVerificationKey(index: Int): ExtendedPublicKeyEd25519 =
    rootVerificationKey.derive(SoftIndex(index))

  def generateKESKey(index: Int): SymmetricKey = Try {
    val softIndex = SoftIndex(index)
    require(privateKeySet.keySet.contains(softIndex), s"Key set does not contain index ${softIndex.value}")
    require(
      !privateKeySet.keySet.exists(_.value < softIndex.value),
      s"Key set contains indices below ${softIndex.value}"
    )
    val sk_KES = kes.keys.SymmetricKey.newFromSeed(
      blake2b256.hash(privateKeySet(softIndex).leftKey.value.toArray).value,
      registrationSlot + softIndex.value * SymmetricKey.maxKeyTimeSteps,
      privateKeySet(softIndex).sign
    )
    privateKeySet.remove(softIndex)
    sk_KES
  } match {
    case Success(value) => value
    case Failure(e)     => throw new Exception(s"HD KES Key Derivation Failed: $e")
  }

  def getBytes: Array[Byte] = serializer.getBytes(this)

}

object HdKesScheme {

  def apply(totalNumberOfKeys: Int, registrationSlot: Long): HdKesScheme = {
    val skm = ExtendedPrivateKeyEd25519(mnemonic.Entropy.fromUuid(java.util.UUID.randomUUID()), "")
    val skIndex = Array.range(0, totalNumberOfKeys).map(DerivedKeyIndex.soft)
    val pkm = skm.public
    val newScheme = new HdKesScheme(
      registrationSlot,
      pkm,
      mutable.Map(skIndex.zip(skIndex.map(skm.derive(_).toOption.get)).toSeq: _*)
    )
    newScheme
  }

  private case object DeserializeKey

  class Serializer {

    def getBytes(key: HdKesScheme): Array[Byte] = sHdKesScheme(key)

    def fromBytes(input: ByteStream): Any = dHdKesScheme(input)

    private def sHdKesScheme(key: HdKesScheme): Array[Byte] =
      Bytes.concat(
        Longs.toByteArray(key.registrationSlot),
        key.rootVerificationKey.bytes.value.toArray,
        key.rootVerificationKey.chainCode.value.toArray,
        Ints.toByteArray(key.privateKeySet.keySet.size),
        Bytes.concat(
          key.privateKeySet
            .map(entry =>
              Longs.toByteArray(entry._1.value)
              ++ entry._2.leftKey.value.toArray
              ++ entry._2.rightKey.value.toArray
              ++ entry._2.chainCode.value.toArray
            )
            .toIndexedSeq: _*
        )
      )

    private def dHdKesScheme(stream: ByteStream): HdKesScheme = {
      val out1 = stream.getLong
      val out2 = SizedBytes[ByteVector32].fit(stream.get(ByteVector32.size), ByteOrdering.LittleEndian)
      val out3 = SizedBytes[ByteVector32].fit(stream.get(ByteVector32.size), ByteOrdering.LittleEndian)
      val out4len = stream.getInt
      var i = 0
      val out4: mutable.Map[SoftIndex, ExtendedPrivateKeyEd25519] = mutable.Map.empty
      while (i < out4len) {
        val index = SoftIndex(stream.getLong)
        val leftKey = SizedBytes[ByteVector32].fit(stream.get(ByteVector32.size), ByteOrdering.LittleEndian)
        val rightKey = SizedBytes[ByteVector32].fit(stream.get(ByteVector32.size), ByteOrdering.LittleEndian)
        val chainCode = SizedBytes[ByteVector32].fit(stream.get(ByteVector32.size), ByteOrdering.LittleEndian)
        val sk = new ExtendedPrivateKeyEd25519(leftKey, rightKey, chainCode)
        out4 += (index -> sk)
        i += 1
      }
      assert(stream.empty)
      HdKesScheme(out1, ExtendedPublicKeyEd25519(out2, out3), out4)
    }
  }

  private class ByteStream(var data: Array[Byte], co: Any) {

    def get(n: Int): Array[Byte] = {
      require(n <= data.length, "Error: ByteStream reached early end of stream")
      val out = data.take(n)
      data = data.drop(n)
      out
    }

    def getAll: Array[Byte] = {
      val out = data
      data = Array()
      out
    }

    def getInt: Int =
      Ints.fromByteArray(get(4))

    def getLong: Long =
      Longs.fromByteArray(get(8))
    def empty: Boolean = data.isEmpty
    def length: Int = data.length
    def caseObject: Any = co
  }

  val serializer: Serializer = new Serializer

  def deserialize(bytes: Array[Byte]): HdKesScheme = {
    val byteStream = new ByteStream(bytes, None)
    val numBytes = byteStream.getInt
    serializer.fromBytes(
      new ByteStream(byteStream.get(numBytes), DeserializeKey)
    ) match { case kd: HdKesScheme => kd }
  }

}
