package co.topl.stakeholder

import akka.util.Timeout
import co.topl.cases._
import co.topl.history.BlockStorage
import co.topl.primitives.{ActorRefWrapper, Parameters}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.math.BigInt

/**
  * AMS 2020:
  * Local Akka message passing routines using ActorRefWrapper,
  * Provides some utilities for coordinator to collect info about state from Stakeholders
  */

trait Messages extends Members {

  override def scheduleDiffuse(): Unit = {
    timers.startPeriodicTimer(Diffuse,Diffuse,10*slotT.millis)
  }

  /**
    * picks set of gossipers randomly
    * @param self ref not to include
    * @param holders list of holders
    * @return list of gossipers
    */

  def gossipSet(self:ActorRefWrapper,
                holders:List[ActorRefWrapper]):List[ActorRefWrapper] = {
    rng.shuffle(holders.filter(ref => ref != self)).take(numGossipers)
  }

  def gossipSet(self:ActorRefWrapper,
                sender:ActorRefWrapper,
                holders:List[ActorRefWrapper]):List[ActorRefWrapper] = {
    rng.shuffle(holders.filter(ref => ref != self && ref != sender)).take(numGossipers)
  }

  /**
    * Sends command to one of the stakeholders
    * @param ref actor list
    * @param command object to be sent
    */

  def send(sender:ActorRefWrapper, ref:ActorRefWrapper, command: Any): Unit = {
    if (useRouting && !useFencing) {
      if (ref.remote) {
        routerRef ! MessageFromLocalToRemote(sender,ref.path, command)
      } else {
        localRef ! MessageFromLocalToLocal(sender, ref, command)
      }
    } else if (useFencing) {
      routerRef ! MessageFromLocalToLocalId(BigInt(fch.hash(rng.nextString(64))),sender,ref,command)
    } else {
      ref ! command
    }
  }

  /**
    * Sends commands one by one to list of stakeholders
    * @param holders actor list
    * @param command object to be sent
    */

  def send(sender:ActorRefWrapper, holders:List[ActorRefWrapper], command: Any): Unit = {
    for (holder <- holders){
      if (useRouting && !useFencing) {
        if (holder.remote) {
          routerRef ! MessageFromLocalToRemote(sender,holder.path, command)
        } else {
          localRef ! MessageFromLocalToLocal(sender, holder, command)
        }
      } else if (useFencing) {
        routerRef ! MessageFromLocalToLocalId(BigInt(fch.hash(rng.nextString(64))),sender,holder,command)
      } else {
        holder ! command
      }
    }
  }

  /**
    * Sends commands one by one to list of stakeholders
    * @param holders actor list
    * @param command object to be sent
    */

  def sendAssertDone(holders:List[ActorRefWrapper], command: Any): Unit = {
    for (holder <- holders){
      implicit val timeout:Timeout = Timeout(waitTime)
      val future = holder ? command
      val result = Await.result(future, timeout.duration)
      assert(result == "done")
    }
  }

  /**
    * Sends command to stakeholder and waits for response
    * @param holder to send to
    * @param command any command
    */

  def sendAssertDone(holder:ActorRefWrapper, command: Any): Unit = {
    implicit val timeout:Timeout = Timeout(waitTime)
    val future = holder ? command
    val result = Await.result(future, timeout.duration)
    assert(result == "done")
  }

  /**
    * returns the staking state to the coordinator
    * @param holder holder to return
    * @return
    */

  def getStakingState(holder:ActorRefWrapper):State = {
    implicit val timeout:Timeout = Timeout(waitTime)
    val future = holder ? RequestState
    val result = Await.result(future, timeout.duration)
    result match {
      case value:GetState =>
        value.s
    }
  }

  /**
    * sets the local chain history and block data to the holders
    * @param holder actor to get data from
    */

  def blockTree(holder:ActorRefWrapper): Unit = {
    implicit val timeout:Timeout = Timeout(waitTime)
    val future = holder ? RequestBlockTree
    val result = Await.result(future, timeout.duration)
    result match {
      case value:GetBlockTree =>
        value.t match {
          case _:BlockStorage => blocks
          case _ => println("error")
        }
        value.h match {
          //case h:SlotHistoryStorage => chainHistory.copy(h)
          case _ => //println("error")
        }
      case _ => println("error")
    }
  }

  def getPositionData(router:ActorRefWrapper):(Map[ActorRefWrapper,(Double,Double)],Map[(ActorRefWrapper,ActorRefWrapper),Long]) = {
    implicit val timeout:Timeout = Timeout(waitTime)
    val future = router ? RequestPositionData
    val result = Await.result(future, timeout.duration)
    result match {
      case value:GetPositionData => value.s
    }
  }
}
