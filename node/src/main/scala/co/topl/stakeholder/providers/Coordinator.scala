package co.topl.stakeholder.providers

import akka.actor.{Actor, Props}
import co.topl.settings.AppSettings
import co.topl.stakeholder.primitives.NTPClient
import co.topl.stakeholder.cases.GetTime
import scala.util.{Failure, Success, Try}
import co.topl.stakeholder.primitives.TetraParameters


class Coordinator extends Actor {

  var localClockOffset:Long = 0

  syncGlobalClock()

  def globalTime:Long = {
    System.currentTimeMillis()+localClockOffset
  }

  def syncGlobalClock(): Unit = {
    var notSynced = true
    while (notSynced) Try{
      val ntpClient = new NTPClient
      ntpClient.getOffset(Array(TetraParameters.timeServer))
    } match {
      case Success(value) =>
        localClockOffset = value
        notSynced = false
      case Failure(_) =>
        println("Error: could not fetch global time, trying again...")
    }

  }

  def giveTime:Receive = {
    /**returns offset time to stakeholder that issues GetTime to coordinator*/
    case GetTime =>
      sender() ! GetTime(globalTime)
  }

  override def receive:Receive = giveTime
}

object Coordinator {
  def props(ref:Seq[akka.actor.ActorRef],settings:AppSettings): Props =
    Props(new Coordinator)
      .withDispatcher(TetraParameters.coordinatorEC)
}
