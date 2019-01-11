package bifrost.inflation

import akka.actor.Actor

import scala.concurrent.duration._
import akka.event.Logging
import org.apache.commons.lang3.exception.ExceptionContext

import scala.concurrent.ExecutionContext

class InflationQuery extends Actor {

  val log = Logging(context.system, this)
  var infVal = 1L
  var infUpdateVal = 1L
  var infUpdateBlock = 1L
  implicit val ec = ExecutionContext.global // gives the actor a context so we can use the scheduler
  self ! constantQuery() // starts the scheduled updater

  @throws(classOf[java.io.IOException])
  private def get(url: String)= scala.io.Source.fromURL(url).mkString

  private def getUpdatedVals() = {
    try {
      //print("starting update call\n")
      val resp = get("http://167.99.135.150/") // returns a string "infVal,###,infUpdateVal,###,infUpdateBlock,###"
      val parts = resp.split(',')
      //print(resp + "\n")
      if (parts.length == 6 && parts(0) == "infVal" && parts(2) == "infUpdateVal" && parts(4) == "infUpdateBlock") { // TODO | clean this up to be more robust
        infVal = parts(1).toLong
        infUpdateVal = parts(3).toLong
        infUpdateBlock = parts(5).toLong
      }
      //print("update call complete: infVal " + infVal + ", infUpdateVal " + infUpdateVal + ", infUpdateBlock " + infUpdateBlock + "\n")
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
      print("\n\nblock height being used to check inflation val: " + x + "\n")
      print("infUpdateBlock: " + infUpdateBlock + "\n")
      if (x.asInstanceOf[java.lang.Long] >= infUpdateBlock) {
        print("returning infUpdateVal : " + infUpdateVal + "\n")
        sender() ! infUpdateVal
      } else {
        print("returning infVal : " + infVal + "\n")
        sender() ! infVal
      }
  }

}
