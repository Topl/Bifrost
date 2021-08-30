package co.topl.stakeholder.cases

import akka.actor.ActorPath
import co.topl.stakeholder.components.{Block, Transaction}
import co.topl.stakeholder.primitives.Types._
import co.topl.stakeholder.primitives.{ActorRefWrapper, KeyFile}

import scala.concurrent.duration.FiniteDuration
import scala.math.BigInt

/**
  * AMS 2020:
  * Case objects and classes for pattern matching messages between actors
  */

case object Diffuse
case object Inbox
case object CloseDataFile
case object Status
case object Run
case object GetTime
case object Update
case object WriteFile
case object StallActor
case object ReadCommand
case object Verify
case object RequestState
case object RequestBlockTree
case object Populate
case object NewDataFile
case object NextSlot
case object EndStep
case object RequestPositionData
case object GetBalance
case object Refresh
case object Register
case object BootstrapJob

//authenticated messages that are communicated remotely and locally between stakeholders
case class DiffuseData(sid:Sid,ref:ActorRefWrapper,pks:PublicKeys,sender:ActorRefWrapper)
case class Hello(slot:Slot,sender:ActorRefWrapper)
case class RequestBlock(id:SlotId,job:Int,sender:ActorRefWrapper)
case class RequestTine(id:SlotId, depth:Int,job:Int,sender:ActorRefWrapper)
case class ReturnBlocks(blocks:List[Block],job:Int,sender:ActorRefWrapper)
case class SendBlock(block:Block,sender:ActorRefWrapper)
case class SendTx(transaction:Transaction,sender:ActorRefWrapper)

//messages between coordinator/router and holders
case class DelayModelMessage(delay:FiniteDuration,nonce:Hash,msg:Any)
case class NewHolderFromUI(kf:KeyFile,dir:String,pswd:String,name:String,kdir:String)
case class MessageFromLocalToRemote(s:ActorRefWrapper,r:ActorPath,c:Any,time:Option[Long] = None)
case class MessageFromLocalToLocal(s:ActorRefWrapper,r:ActorRefWrapper,c:Any)
case class MessageFromLocalToLocalId(uid:BigInt,s:ActorRefWrapper,r:ActorRefWrapper,c:Any)
case class HoldersFromLocal(list:List[ActorRefWrapper],printInfo:Boolean)
case class HoldersWithPosition(list:List[ActorRefWrapper],pos:Map[ActorRefWrapper,(Double,Double)])
case class HoldersFromRemote(list:List[ActorRefWrapper])
case class Flag(s:ActorRefWrapper,f:String)
case class GetSlot(s:Int)
case class CoordRef(ref: ActorRefWrapper)
case class GetTime(t1:Long)
case class SetClock(t0:Long)
case class GenBlock(b: Block)
case class IssueTx(ref:ActorRefWrapper,delta:BigInt)
case class IssueTxToAddress(recip:PublicKeyW,delta:BigInt)
case class WriteFile(fw: Any)
case class NewGraphFile(name:String)
case class GetGossipers(list:List[ActorRefWrapper])
case class Party(list:List[ActorRefWrapper],clear:Boolean)
case class GetState(s:State)
case class GetBlockTree(t:Any,h:Any)
case class GetPositionData(s:(Map[ActorRefWrapper,(Double,Double)],Map[(ActorRefWrapper,ActorRefWrapper),Long]))
case class Adversary(s:String)
case class Initialize(gs:Slot,password:Option[String])
case class GuiCommand(s:String)
case class RouterPeerInfo(pathToPeer:Map[ActorPath,(String,PublicKey,Long)],
                          pathsToRemove:Seq[ActorPath],
                          bootStrapJobs:Set[ActorRefWrapper],
                          holders:List[ActorRefWrapper])
case class BootstrapJob(ref:ActorRefWrapper)


