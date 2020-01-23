package bifrost.inflation

import akka.actor.Actor

import scala.concurrent.duration._
import akka.event.Logging

import scala.concurrent.ExecutionContext

class InflationQuery extends Actor {

  val log = Logging(context.system, this)
  var infVal = 1L
  var infUpdateVal = 1L
  var infUpdateBlock = 1L
  implicit val ec = ExecutionContext.global // gives the actor a context so we can use the scheduler
  //self ! constantQuery() // starts the scheduled updater

  @throws(classOf[java.io.IOException])
  private def get(url: String)= scala.io.Source.fromURL(url).mkString

  private def getUpdatedVals() = {
    try {
      //TODO Replace inflation query server
      val resp = get("") // returns a string "infVal,###,infUpdateVal,###,infUpdateBlock,###"
      val parts = resp.split(',')
      if (parts.length == 6 && parts(0) == "infVal" && parts(2) == "infUpdateVal" && parts(4) == "infUpdateBlock") {
        infVal = parts(1).toLong
        infUpdateVal = parts(3).toLong
        infUpdateBlock = parts(5).trim.toLong
      }
    } catch {
      case e: Exception => print(e + "\n")
    }
  }

  private def constantQuery() = {
    getUpdatedVals()
    context.system.scheduler.scheduleOnce(60 seconds, self, "constantQuery") // calls self after 1 minute delay
  }

  override def receive: Receive = {
    case "constantQuery" => // prompts actor to endlessly update
      constantQuery()
    case "getUpdatedVals" => // prompting val update
      getUpdatedVals()
    case x: Long =>
      if (x.asInstanceOf[java.lang.Long] >= infUpdateBlock) {
        sender() ! infUpdateVal
      } else {
        sender() ! infVal
      }
  }

}
