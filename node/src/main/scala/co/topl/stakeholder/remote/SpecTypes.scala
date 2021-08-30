package co.topl.stakeholder.remote

import co.topl.stakeholder.primitives.Types._
import co.topl.stakeholder.primitives.Mac
import co.topl.stakeholder.components.{Block,Transaction}

/**
  * AMS 2020:
  * Message codes used for Prosomo, message data types for serialization
  */

object SpecTypes {
  /*
  used message codes in bifrost:
  1,22,65,55,33,2
   */

  //Diffuse message type for diffusing public keys, not used in Genesis, only for estimate of active stake
  type DiffuseDataType = (String,String,String,Sid,PublicKeys)

  //Hello message for bootstrapping peers, fetch information functionality
  type HelloDataType = (String,String,Slot)

  //Tinepool request block
  type RequestBlockType = (String,String,SlotId,Int)

  //Tinepool request tine
  type RequestTineType = (String,String,SlotId,Int,Int)

  //Tinepool returned block
  type ReturnBlocksType = (String,String,List[Block],Int)

  //New blocks to be gossiped
  type SendBlockType = (String,String,Block)

  //New Txs to be gossiped
  type SendTxType = (String,String,Transaction)

  //Establish session key and actor paths
  type HoldersType = (List[String],PublicKey,Long)

  //Flood: ingress -> if new entry then egress multicast
  val diffuseCode:Byte = 100:Byte

  //P2P: ingress unicast -> egress unicast
  val helloCode:Byte = 101:Byte

  //P2P: ingress unicast -> egress unicast
  val requestBlockCode:Byte = 102:Byte

  //P2P: ingress unicast -> egress unicast
  val requestTineCode:Byte = 103:Byte

  //P2P: ingress unicast -> egress unicast
  val returnBlocksCode:Byte = 104:Byte

  //Flood: ingress -> if new entry then egress multicast
  val sendBlockCode:Byte = 105:Byte

  //Flood: ingress -> if new entry then egress multicast
  val sendTxCode:Byte = 106:Byte

  //P2P: ingress unicast -> egress unicast
  val holdersFromRemote:Byte = 107:Byte

}
