package bifrost.inflation

import akka.actor.Actor
import akka.event.Logging
import scalapb.descriptors.ScalaType.Message

case object InflationQuery

class InflationQuery extends Actor {

  val log = Logging(context.system, this)
  var infVal = 1L
  var infUpdateVal = 1L
  var infUpdateBlock = 1L

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

  override def receive: Receive = {
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
      getUpdatedVals()
  }

}
