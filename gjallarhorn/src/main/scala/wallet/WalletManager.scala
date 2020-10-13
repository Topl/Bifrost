package wallet

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.{Timeout}
import io.circe.{Json, ParsingFailure}
import io.circe.parser.parse
import utils.Logging

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.Await
import scala.concurrent.duration._


class WalletManager(publicKeys: Set[String]) extends Actor with Logging {

  import WalletManager._

  implicit val timeout: Timeout = 10.seconds

  /**
    * Represents the wallet boxes: publicKey1 -> {id1 -> walletBox1, id2 -> walletBox2, ...}, publicKey2 -> {},...
    */
  var walletBoxes: MMap[String, MMap[String, Json]] = {
    val returnVal: MMap[String, MMap[String, Json]] = MMap.empty
    publicKeys.map(key =>
      returnVal.put(key.toString, MMap.empty)
    )
    returnVal
  }

  var bifrostActorRef: Option[ActorRef] = None

  var newestBlock: Option[String] = None

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.info("WalletManagerActor: preRestart")
    log.info(s"WalletManagerActor reason: ${reason.getMessage}")
    log.info(s"WalletManagerActor message: ${message.getOrElse("")}")
    super.preRestart(reason, message)
  }

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

  def newBlock(blockMsg: String): Unit = {
    val block : String = blockMsg.substring(16)
    log.info(s"Wallet Manager received new block: ${block}")
    parse(block) match {
      case Right(blockJson) => {
        updateWallet(blockJson)
      }
      case Left(e) => sys.error(s"Could not parse json $e")
    }
    newestBlock = Some(block)
  }

  def updateWallet(newBlock: Json): Unit = {
      val tx = (newBlock \\ "txs").head
      val newBoxes = tx \\ "newBoxes"
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

  def gjalStart(bifrost: ActorRef): Unit = {
    bifrostActorRef = Some(bifrost)
    context.system.eventStream.subscribe(bifrost, classOf[String])
    bifrost ! "Remote wallet actor initialized"
    /*val response: String = Await.result((bifrost ? "Remote wallet actor initialized").mapTo[String], 10.seconds)
    sender ! response*/
  }

  /*case msg: String => {
    if (msg.contains("New block added")) {
    println(s"Wallet Manager received block: ${msg.substring(17)}")
    newestBlock = Some(msg.substring(17))
  }
    if (msg.contains("new block added")) {
    println(s"Wallet Manager received block from generic node view holder: ${msg.substring(17)}")
    newestBlock = Some(msg.substring(17))
  }
    log.info(s"${Console.MAGENTA} Received a message: $msg")
  }*/

  def msgHandling(msg: String): Unit = {
    if (msg.contains("received new wallet from:")) {
      log.info(s"${Console.YELLOW} Bifrost $msg")
    }
    if (msg.contains("new block added")) {
      newBlock(msg)
    }
    if (msg.contains("publishing")) {
      log.info(s"published block!")
    }
  }


  override def receive: Receive = {
    case GjallarhornStarted(actorRef: ActorRef) => {
      gjalStart(actorRef)
    }

    case msg: String => {
      msgHandling(msg)
    }

    case GetNewBlock => {
      sender ! newestBlock
    }

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
    case GjallarhornStopped => {
      bifrostActorRef match {
        case Some(actorRef) =>
          val response: String = Await.result((actorRef ? "Remote wallet actor stopped").mapTo[String], 10.seconds)
          sender ! response
        case None => log.error("actor ref was not found.")
      }}
  }
}

object WalletManager {

  /**
    * Given the updated boxes, updates the walletboxes and returns the updated walletboxes
    * @param updatedBoxes - the current balances from Bifrost
    */
  case class UpdateWallet(updatedBoxes: Json)
  //case class UpdateWallet(add: MMap[String, MMap[String, Json]], remove: List[(String, List[String])])

  case class GjallarhornStarted(bifrostActorRef: ActorRef)
  case object GjallarhornStopped
  case object GetNewBlock
  case class NewBlock(block: String)

}
