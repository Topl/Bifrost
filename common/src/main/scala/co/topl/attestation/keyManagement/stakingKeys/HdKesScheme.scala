package co.topl.attestation.keyManagement.stakingKeys

import co.topl.attestation.SignatureEd25519
import co.topl.crypto.kes
import co.topl.attestation.keyManagement.derivedKeys.{DerivedKeyIndex, ExtendedPrivateKeyEd25519, ExtendedPublicKeyEd25519, SoftIndex}
import co.topl.attestation.keyManagement.mnemonic
import co.topl.attestation.keyManagement.stakingKeys.HdKesScheme.serializer
import co.topl.crypto.kes.keys.SymmetricKey
import com.google.common.primitives.{Bytes, Ints, Longs}
import scodec.bits.{ByteOrdering, ByteVector}
import co.topl.utils.SizedBytes
import co.topl.utils.SizedBytes.Types.ByteVector32
import co.topl.utils.SizedBytes.implicits._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class HdKesScheme(rootVerificationKey: ExtendedPublicKeyEd25519, privateKeySet: mutable.Map[SoftIndex, ExtendedPrivateKeyEd25519]) {

  def deriveVerificationKey(index: SoftIndex): ExtendedPublicKeyEd25519 =
    rootVerificationKey.derive(index)

  def generateKESKey(index: SoftIndex, previousOffset: Long): (SignatureEd25519, SymmetricKey) = Try{
    require(privateKeySet.keySet.contains(index),s"Key set does not contain index ${index.value}")
    require(!privateKeySet.keySet.exists(_.value < index.value),s"Key set contains indices below ${index.value}")
    val sk_KES = kes.keys.SymmetricKey.newFromSeed(
      privateKeySet(index).leftKey.value.toArray,
      previousOffset + index.value * SymmetricKey.maxKeyTimeSteps
    )
    val vk_KES = sk_KES.getVerificationKey
    val sign_vk_KES = privateKeySet(index).sign(vk_KES.bytes)
    privateKeySet.remove(index)
    (sign_vk_KES, sk_KES)
  } match {
    case Success(value) => value
    case Failure(e) => throw new Exception(s"HD KES Key Derivation Failed: $e")
  }

  def getBytes: Array[Byte] = serializer.getBytes(this)

}

object HdKesScheme {

  def apply(totalNumberOfKeys: Int): HdKesScheme = {
    val skm = ExtendedPrivateKeyEd25519(mnemonic.Entropy.fromUuid(java.util.UUID.randomUUID()), "")
    val skIndex = Array.range(0, totalNumberOfKeys).map(DerivedKeyIndex.soft)
    val pkm = skm.public
    val newScheme = new HdKesScheme(
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
        key.rootVerificationKey.bytes.value.toArray,
        key.rootVerificationKey.chainCode.value.toArray,
        Ints.toByteArray(key.privateKeySet.keySet.size),
        Bytes.concat(
          key.privateKeySet.map(
            entry =>
              Longs.toByteArray(entry._1.value)
                ++ entry._2.leftKey.value.toArray
                ++ entry._2.rightKey.value.toArray
                ++ entry._2.chainCode.value.toArray
          ).toIndexedSeq:_*
        )
      )

    private def dHdKesScheme(stream: ByteStream): HdKesScheme = {
      val out1 = SizedBytes[ByteVector32].fit(stream.get(ByteVector32.size), ByteOrdering.LittleEndian)
      val out2 = SizedBytes[ByteVector32].fit(stream.get(ByteVector32.size), ByteOrdering.LittleEndian)
      val out3len = stream.getInt
      var i = 0
      val out3:mutable.Map[SoftIndex,ExtendedPrivateKeyEd25519] = mutable.Map.empty
      while (i < out3len) {
        val index = SoftIndex(stream.getLong)
        val leftKey = SizedBytes[ByteVector32].fit(stream.get(ByteVector32.size), ByteOrdering.LittleEndian)
        val rightKey = SizedBytes[ByteVector32].fit(stream.get(ByteVector32.size), ByteOrdering.LittleEndian)
        val chainCode = SizedBytes[ByteVector32].fit(stream.get(ByteVector32.size), ByteOrdering.LittleEndian)
        val sk = new ExtendedPrivateKeyEd25519(leftKey,rightKey,chainCode)
        out3 += (index -> sk)
        i += 1
      }
      assert(stream.empty)
      HdKesScheme(ExtendedPublicKeyEd25519(out1,out2),out3)
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
