package co.topl.crypto.kes

import co.topl.crypto.hash.blake2b256
import co.topl.crypto.kes
import co.topl.crypto.kes.keys.SymmetricKey
import co.topl.crypto.mnemonic.Entropy
import co.topl.crypto.typeclasses._
import co.topl.crypto.typeclasses.implicits._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, Proofs, SecretKeys, VerificationKeys}
import com.google.common.primitives

import java.util.UUID
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

case class HdKesScheme(
  registrationSlot:    Long,
  rootVerificationKey: VerificationKeys.ExtendedEd25519,
  privateKeySet:       mutable.Map[Derivative.KeyIndexes.Soft, SecretKeys.ExtendedEd25519]
) {

  import HdKesScheme.serializer

  def deriveVerificationKey(index: Int): VerificationKeys.ExtendedEd25519 =
    rootVerificationKey.softDerive(Derivative.KeyIndexes.Soft(index))

  def generateKESKey(index: Int): Nothing = Try {
    val softIndex = Derivative.KeyIndexes.Soft(index)
    require(privateKeySet.keySet.contains(softIndex), s"Key set does not contain index ${softIndex.value}")
    require(
      !privateKeySet.keySet.exists(_.value < softIndex.value),
      s"Key set contains indices below ${softIndex.value}"
    )
    val sk_KES = kes.keys.SymmetricKey.newFromSeed(
      blake2b256.hash(privateKeySet(softIndex).leftKey.data.toArray).value,
      registrationSlot + softIndex.value * SymmetricKey.maxKeyTimeSteps,
      bytes =>
        Prover[SecretKeys.ExtendedEd25519, Proofs.Signature.Ed25519].proveWith(privateKeySet(softIndex), bytes.toArray)
    )
    privateKeySet.remove(softIndex)
    sk_KES
  } match {
    case Success(value) => ???
    case Failure(e)     => throw new Exception(s"HD KES Key Derivation Failed: $e")
  }

  def getBytes: Array[Byte] = serializer.getBytes(this)

}

object HdKesScheme {

  def apply(totalNumberOfKeys: Int, registrationSlot: Long): HdKesScheme = {
    implicit val entropy: Entropy = Entropy.fromUuid(UUID.randomUUID())
    val skm = KeyInitializer[SecretKeys.ExtendedEd25519].random()
    val skIndex = Array.range(0, totalNumberOfKeys).map(idx => Derivative.KeyIndexes.Soft(idx.toLong))
    val pkm =
      ContainsVerificationKey[SecretKeys.ExtendedEd25519, VerificationKeys.ExtendedEd25519].verificationKeyOf(skm)
    new HdKesScheme(
      registrationSlot,
      pkm,
      mutable.Map.from(skIndex.map(idx => idx -> skm.softDerive(idx)))
    )
  }

  private case object DeserializeKey

  class Serializer {

    def getBytes(key: HdKesScheme): Array[Byte] = sHdKesScheme(key)

    def fromBytes(input: ByteStream): Any = dHdKesScheme(input)

    private def sHdKesScheme(key: HdKesScheme): Array[Byte] =
      primitives.Bytes.concat(
        primitives.Longs.toByteArray(key.registrationSlot),
        ???,
        key.rootVerificationKey.chainCode.data.toArray,
        primitives.Ints.toByteArray(key.privateKeySet.keySet.size),
        primitives.Bytes.concat(
          key.privateKeySet
            .map(entry =>
              primitives.Longs.toByteArray(entry._1.value)
              ++ entry._2.leftKey.data.toArray
              ++ entry._2.rightKey.data.toArray
              ++ entry._2.chainCode.data.toArray
            )
            .toIndexedSeq: _*
        )
      )

    private def dHdKesScheme(stream: ByteStream): HdKesScheme = {
      val out1 = stream.getLong
      val out2 = stream.get(32)
      val out3 = stream.get(32)
      val out4len = stream.getInt
      var i = 0
      val out4: mutable.Map[Derivative.KeyIndexes.Soft, SecretKeys.ExtendedEd25519] = mutable.Map.empty
      while (i < out4len) {
        val index = Derivative.KeyIndexes.Soft(stream.getLong)
        val leftKey = Bytes(stream.get(32))
        val rightKey = Bytes(stream.get(32))
        val chainCode = Bytes(stream.get(32))
        val sk = SecretKeys.ExtendedEd25519(
          Sized.strictUnsafe(leftKey),
          Sized.strictUnsafe(rightKey),
          Sized.strictUnsafe(chainCode)
        )
        out4 += (index -> sk)
        i += 1
      }
      assert(stream.empty)
      HdKesScheme(
        out1,
        VerificationKeys
          .ExtendedEd25519(VerificationKeys.Ed25519(Sized.strictUnsafe(Bytes(out2))), Sized.strictUnsafe(Bytes(out3))),
        out4
      )
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
      primitives.Ints.fromByteArray(get(4))

    def getLong: Long =
      primitives.Longs.fromByteArray(get(8))
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
