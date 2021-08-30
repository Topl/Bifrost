package co.topl.stakeholder.history

import co.topl.stakeholder.primitives.{ByteStream, Fch, LDBStore, Types, Ratio}
import co.topl.stakeholder.primitives.ByteArrayWrapper
import co.topl.stakeholder.components.Serializer

/**
  * AMS 2020:
  * Storage for tine structure information and research metrics
  */


class SlotHistoryStorage(dir:String) extends Types {
  import co.topl.stakeholder.components.Serializer._
  val fch = new Fch

  private val blockReorgStore:LDBStore = LDBStore(s"$dir/history/tree")

  def update(slotId:SlotId,parentId:SlotId,threshold:Ratio,number:Int,numTx:Int,serializer:Serializer):Unit = {
    val blockSlotHash = hash(slotId._1,serializer)
    val slotList = get(blockSlotHash,serializer)
    blockReorgStore.update(Seq(),Seq(blockSlotHash -> ByteArrayWrapper(serializer.getBytes(slotId._2::slotList))))
  }

  def get(slot:Slot,serializer: Serializer):List[BlockId] = {
    val blockSlotHash = hash(slot,serializer)
    blockReorgStore.get(blockSlotHash) match {
      case Some(bytes: ByteArrayWrapper) => {
        val byteStream = new ByteStream(bytes.data,DeserializeIdList)
        serializer.fromBytes(byteStream) match {case idl:List[BlockId]@unchecked => idl}
      }
      case None => List(ByteArrayWrapper(Array()))
    }
  }

  def get(blockSlotHash:Hash,serializer: Serializer):List[BlockId] = {
    blockReorgStore.get(blockSlotHash) match {
      case Some(bytes: ByteArrayWrapper) => {
        val byteStream = new ByteStream(bytes.data,DeserializeIdList)
        serializer.fromBytes(byteStream) match {case idl:List[BlockId]@unchecked => idl}
      }
      case _ => List(ByteArrayWrapper(Array()))
    }
  }
}
