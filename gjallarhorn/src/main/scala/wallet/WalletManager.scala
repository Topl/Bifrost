package wallet

import akka.actor.{Actor, ActorRef, ActorSelection}
import akka.pattern.ask
import akka.util.Timeout
import io.circe.{Json, ParsingFailure}
import io.circe.parser.parse

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.Await
import scala.concurrent.duration._


class WalletManager(publicKeys: Set[String]) extends Actor {

  import WalletManager._

  implicit val timeout: Timeout = 10.seconds

  var walletBoxes: MMap[String, MMap[String, Json]] = {
    val returnVal: MMap[String, MMap[String, Json]] = MMap.empty
    publicKeys.map(key =>
      returnVal.put(key.toString, MMap.empty)
    )
    returnVal
  }

  val bifrostActorRef: ActorSelection = context.actorSelection("akka.tcp://bifrost-client@127.0.0.1:9087/user/walletActorManager")

  /**
    * Parses the list of boxes for a specific type (asset, poly, or arbit)
    * @param sameTypeBoxes - list of boxes all of the same box type
    * @return a map of the box id mapped to the box info (as json)
    */
  def parseBoxType (sameTypeBoxes: Json): MMap[String, Json] = {
    val boxesMap: MMap[String, Json] = MMap.empty
    var boxesArray: Array[String] = sameTypeBoxes.toString().trim.stripPrefix("[").stripSuffix("]").
      split("},")
    boxesArray = boxesArray.map(asset => {
      if (boxesArray.indexOf(asset) != boxesArray.size-1) {
        asset.concat("}")
      } else asset
    })
    boxesArray.foreach(asset => {
      val assetJson: Either[ParsingFailure, Json] = parse(asset)
      assetJson match {
        case Right(json) => {
          val id = (json \\ "id").head.toString()
          boxesMap.put(id, json)
        }
        case Left(e) => sys.error(s"Could not parse json: $e")
      }
    })
    boxesMap
  }

  /**
    * Given the balance response from Bifrost, parse the json and update wallet box
    * @param json - the balance response from Bifrost
    * @return - the updated walletBoxes
    */
  def parseAndUpdate(json: Json): MMap[String, MMap[String, Json]] = {
    val pubKeys: scala.collection.Set[String] = walletBoxes.keySet
    pubKeys.foreach(key => {
      val info: Json = (json \\ key).head
      var boxesMap: MMap[String, Json] = MMap.empty
      val boxes = info \\ "Boxes"
      if (boxes.nonEmpty) {
        val assets: List[Json] = boxes.head \\ "Asset"
        val poly: List[Json] = boxes.head \\ "Poly"
        val arbit: List[Json] = boxes.head \\ "Arbit"
        if (assets.nonEmpty) {
          boxesMap = parseBoxType(assets.head)
        }
        if (poly.nonEmpty) {
          boxesMap = boxesMap ++ parseBoxType(poly.head)
        }
        if (arbit.nonEmpty) {
          boxesMap = boxesMap ++ parseBoxType(arbit.head)
        }
        walletBoxes(key) = boxesMap}
    })
    walletBoxes
  }

  override def receive: Receive = {

    case UpdateWallet(updatedBoxes) => {
      sender ! parseAndUpdate(updatedBoxes)
    }

    /*case UpdateWallet(add, remove) => {
      remove.foreach { case (publicKey, ids) =>
        walletBoxes.get(publicKey).map(boxes => ids.foreach(boxes.remove))
      }
      add.foreach { case (publicKey, newBoxes) =>
        walletBoxes.get(publicKey).map(boxes => newBoxes.foreach(box => boxes.put(box._1, box._2)))
      }
    }*/

    case msg: String => {
      if (msg.contains("new block added:")) {
        println(s"Wallet Manager received block: ${msg.substring(17)}")
      }

    }

    case GjallarhornStarted() => {
      val response: String = Await.result((bifrostActorRef ? "Remote wallet actor initialized").mapTo[String], 10.seconds)
      sender ! response
    }
  }
}

object WalletManager {

  /**
    * Given the updated boxes, updates the walletboxes and returns the updated walletboxes
    * @param updatedBoxes - the current balances from Bifrost
    */
  case class UpdateWallet(updatedBoxes: Json)
  //case class UpdateWallet(add: MMap[String, MMap[String, Json]], remove: List[(String, List[String])])

  case class GjallarhornStarted()
}
