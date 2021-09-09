package co.topl.stakeholder.primitives

import co.topl.stakeholder.primitives.ByteArrayWrapper
import co.topl.stakeholder.components.Transaction
import scala.collection.mutable
import scala.math.BigInt

/**
 * AMS 2020:
 * Shared types trait referenced project wide,
 * You will likely need to include this if you want to interact with any consensus components
 */

trait SimpleTypes {

  val pk_length: Int = 32
  val pkw_length: Int = pk_length * 3
  val sig_length: Int = 64
  val hash_length: Int = 32
  val eta_length: Int = 32
  val int_length: Int = 4
  val long_length: Int = 8
  val sid_length: Int = 32
  val pi_length: Int = 80
  val rho_length: Int = 64
  val id_length: Int = hash_length
  val mac_length: Int = hash_length + long_length
  val slot_length: Int = int_length
  val bn_length: Int = int_length
  val sk_length: Int = 32

  type Hash = ByteArrayWrapper
  type Eta = Array[Byte]
  type Signature = Array[Byte]
  type Slot = Int
  type BlockNumber = Int
  type Rho = Array[Byte]
  type PublicKey = Array[Byte]
  type Sid = Hash
  type PublicKeyW = ByteArrayWrapper
  type PublicKeys = (PublicKey, PublicKey, PublicKey)
  type PrivateKey = Array[Byte]
  type Pi = Array[Byte]
  type BlockId = Hash
  type SlotId = (Slot, BlockId)
  type Cert = (PublicKey, Rho, Pi, PublicKey, Ratio, String)
  type TransactionSeq = Seq[Transaction]
  type GenesisSeq = Seq[(Array[Byte], ByteArrayWrapper, BigInt)]
  type ForgingSignature = (Array[Byte], Array[Byte], Array[Byte], Int, Array[Byte])
  type BlockHeader = (Hash, Hash, Slot, Cert, Rho, Pi, ForgingSignature, PublicKey, BlockNumber, Slot)
  type Request = (List[SlotId], Int, Int)
  type State = Map[PublicKeyW, (BigInt, Boolean, Int)]
  type MemPool = Map[Sid, (Transaction, Int)]
  type TineData = (mutable.SortedMap[BigInt, SlotId], Slot, Slot)

}
