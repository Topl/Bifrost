package co.topl.stakeholder.primitives

import java.security.MessageDigest

import com.google.common.primitives.Bytes
import co.topl.stakeholder.primitives.ByteArrayWrapper
import co.topl.stakeholder.components.{Serializer, Transaction}

/**
 * AMS 2020:
 * Types and hash functions for quickly hashing different data types
 */

trait Types extends SimpleTypes {
  val fch: Fch

  def Sha512(bytes: Array[Byte]): Array[Byte] = {
    val digest = MessageDigest.getInstance("SHA-512")
    digest.update(bytes)
    digest.digest()
  }

  def hash(input: ActorRefWrapper, slot: Slot, serializer: Serializer): Hash =
    hash(input.path.toString + slot.toString, serializer)

  def hash(input: Slot, serializer: Serializer): Hash =
    hash(input.toString + "SLOT_HASH", serializer)

  def hash(input: (ActorRefWrapper, PublicKeys), serializer: Serializer): Hash =
    ByteArrayWrapper(
      fch.hash(
        Bytes.concat(
          serializer.getBytes(input._1.path.toString),
          input._2._1,
          input._2._2,
          input._2._3
        )
      )
    )

  def hashGenEntry(input: (Array[Byte], ByteArrayWrapper, BigInt), serializer: Serializer): Hash =
    ByteArrayWrapper(
      fch.hash(
        Bytes.concat(
          input._1,
          input._2.data,
          input._3.toByteArray
        )
      )
    )

  def hash(input: BlockHeader, serializer: Serializer): Hash =
    ByteArrayWrapper(fch.hash(serializer.getBytes(input)))

  def hash(input: Transaction, serializer: Serializer): Hash =
    ByteArrayWrapper(fch.hash(serializer.getBytes(input)))

  def hash(input: (List[SlotId], Int, Int), serializer: Serializer): Hash =
    ByteArrayWrapper(
      fch.hash(
        Bytes.concat(
          Bytes.concat(input._1.map(serializer.getBytes): _*),
          serializer.getBytes(input._2),
          serializer.getBytes(input._3)
        )
      )
    )

  def hashGen(input: GenesisSeq, serializer: Serializer): Hash =
    ByteArrayWrapper(
      fch.hash(
        Bytes.concat(input.map(serializer.getBytes): _*)
      )
    )

  def hash(input: TransactionSeq, serializer: Serializer): Hash =
    ByteArrayWrapper(
      fch.hash(
        Bytes.concat(input.map(serializer.getBytes): _*)
      )
    )

  def hash(input: String, serializer: Serializer): Hash =
    ByteArrayWrapper(fch.hash(serializer.getBytes(input)))

}

object Types extends SimpleTypes {
  val fch = new Fch
}
