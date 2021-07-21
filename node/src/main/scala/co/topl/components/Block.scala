package co.topl.components

import io.iohk.iodb.ByteArrayWrapper
import co.topl.primitives.SimpleTypes
import co.topl.primitives.Types.{BlockHeader,GenesisSeq,TransactionSeq}

/**
  * AMS 2020:
  * Block container with optional fields and a unique identifier
  */

case class Block(identifier:ByteArrayWrapper,
                 blockHeader:Option[BlockHeader],
                 blockBody:Option[TransactionSeq],
                 genesisSet: Option[GenesisSeq]
                ) extends SimpleTypes {
  def id:BlockId = identifier
  def slotId:SlotId = (slot,id)
  def parentSlotId:SlotId = {
    val header = blockHeader.get
    (header._10,header._1)
  }
  def tetraHeader:BlockHeader = blockHeader.get
  def pid:BlockId = tetraHeader._1
  def ledger:Hash = tetraHeader._2
  def slot:Slot = tetraHeader._3
  def certificate:Cert = tetraHeader._4
  def nonce:Rho = tetraHeader._5
  def proof:Pi = tetraHeader._6
  def signature:ForgingSignature = tetraHeader._7
  def kes_key:PublicKey = tetraHeader._8
  def number:BlockNumber = tetraHeader._9
  def parentSlot:Slot = tetraHeader._10
}
