package wallet

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import io.circe.{Json, ParsingFailure, parser}
import io.circe.parser.parse
import utils.Logging

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * The WalletManager manages the communication between Bifrost and Gjallarhorn
  * Mainly, the WalletManager receives new blocks from Bifrost in order to updates its wallet boxes.
  * @param publicKeys: the set of publicKeys that the WalletManager should keep track of.
  */
class WalletManager(publicKeys: Set[String], bifrostActorRef: ActorRef) extends Actor with Logging {

  import WalletManager._

  implicit val timeout: Timeout = 10.seconds

  /**
    * Represents the wallet boxes: as a mapping of publicKeys to a map of its id's mapped to walletBox.
    * Ex: publicKey1 -> {id1 -> walletBox1, id2 -> walletBox2, ...}, publicKey2 -> {},...
    */
  var walletBoxes: MMap[String, MMap[String, Json]] = {
    val returnVal: MMap[String, MMap[String, Json]] = MMap.empty
    publicKeys.map(key =>
      returnVal.put(key.toString, MMap.empty)
    )
    returnVal
  }

  var newestTransactions: Option[String] = None

  var connectedToBifrost: Boolean = false

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.debug(s"WalletManagerActor: preRestart ${reason.getMessage}")
    log.debug(s"WalletManagerActor message: ${message.getOrElse("")}")
    super.preRestart(reason, message)
  }

  /**
    * Handles messages received from Bifrost
    * @param msg - handles "received new wallet" or "new block added" messages.
    */
  def msgHandling(msg: String): Unit = {
    if (msg.contains("received new wallet from:")) {
      connectedToBifrost = true
      log.info(s"${Console.YELLOW} Bifrost $msg")
    }
    if (msg.contains("new block added")) {
      newBlock(msg)
    }
  }

  //------------------------------------------------------------------------------------
  //Methods for parsing balance response - UpdateWallet

  /**
    * Parses the list of boxes for a specific type (asset, poly, or arbit)
    * @param sameTypeBoxes - list of boxes all of the same box type
    * @return a map of the box id mapped to the box info (as json)
    */
  def parseBoxType (sameTypeBoxes: Json): MMap[String, Json] = {
    val boxesMap: MMap[String, Json] = MMap.empty
    val boxesArray: Array[String] = parseJsonList(sameTypeBoxes)
    boxesArray.foreach(asset => {
      val assetJson: Either[ParsingFailure, Json] = parse(asset)
      assetJson match {
        case Right(json) =>
          val id = (json \\ "id").head.toString()
          boxesMap.put(id, json)
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

  //------------------------------------------------------------------------------------
  //Methods for parsing new block from Bifrost:

  /**
    * Given a json of a list of newBoxes, parses the json into an array of strings.
    * @param list - the list of newBoxes in json
    * @return - returns an array of strings that represent the list of boxes.
    */
  def parseJsonList(list: Json): Array[String] = {
    var listArray: Array[String] = list.toString().trim.stripPrefix("[").stripSuffix("]").
      split("},")
    listArray = listArray.map(asset => {
      if (listArray.indexOf(asset) != listArray.length-1) {
        asset.concat("}")
      } else asset
    })
    listArray
  }

  /**
    * Parses a new block received from Bifrost and saves it to the "newestBlock" value.
    * @param blockMsg - the json of the new block in string form.
    */
  def newBlock(blockMsg: String): Unit = {
    val blockTxs : String = blockMsg.substring("new block added: ".length)
    log.info(s"Wallet Manager received new block with transactions: $blockTxs")
    updateWalletFromBlock(blockTxs)
    newestTransactions = Some(blockTxs)
  }

  /**
    * Parses through newBoxes and adds the boxes to the map of boxes to add to the wallet.
    * @param newBoxes - newBoxes for a specific transaction.
    * @param add - the current map of boxes to add.
    * @return - the updates map of boxes to add with the newBoxes.
    */
  def parseBoxesToAdd (newBoxes: Json, add: MMap[String, MMap[String, Json]]): MMap[String, MMap[String, Json]] = {
    val boxes: Array[String] = parseJsonList(newBoxes)
    boxes.foreach(boxString => {
      val boxJson: Either[ParsingFailure, Json] = parse(boxString)
      boxJson match {
        case Right(json) =>
          val publicKey: String = (json \\ "proposition").head.toString().replace("\"", "")
          var idToBox: MMap[String, Json] = MMap.empty
          add.get(publicKey) match {
            case Some(boxesMap) => idToBox = boxesMap
            case None => idToBox = MMap.empty
          }
          val id: String = (json \\ "id").head.toString().replace("\"", "")
          idToBox.put(id, json)
          add.put(publicKey, idToBox)
        case Left(e) => sys.error(s"Could not parse json: $e")
      }
    })
    add
  }

  /**
    * Parses through the transactions to update the wallet with new boxes or boxes to remove.
    * @param txs - the transactions from the new block.
    */
  def updateWalletFromBlock(txs: String): Unit = {
    var add: MMap[String, MMap[String, Json]] = MMap.empty
    //val remove: List[(String, List[String])] = List.empty
    var idsToRemove: List[String] = List.empty
    var transactions: Array[String] = txs.trim.stripPrefix("[").stripSuffix("]").split("fee")
    transactions = transactions.map(asset => {
      if (transactions.indexOf(asset) != transactions.length-1) {
        if (transactions.indexOf(asset) != 0) {
          asset.trim.substring(3).trim.substring(2).trim.substring(2).trim
            .substring(0, asset.length-14).trim.stripSuffix(",").concat("}")
        }else asset.substring(0, asset.length-2).trim.stripSuffix(",").concat("}")
      } else asset.trim.substring(3).trim.substring(2).trim
    })
    transactions.foreach(txString => {
      if (txString.length > 1) {
        val txJson: Either[ParsingFailure, Json] = parse(txString)
        txJson match {
          case Right(json) =>
            val newBoxes = (json \\ "newBoxes").head
            add = parseBoxesToAdd(newBoxes, add)
            if ((json \\ "boxesToRemove").nonEmpty) {
              val boxesToRemove: List[Json] = json \\ "boxesToRemove"
              idsToRemove = boxesToRemove.map(box => box.toString().trim.stripPrefix("[").stripSuffix("]").trim.stripPrefix("\"").stripSuffix("\""))
            }
          case Left(e) => println("could not parse: " + txString)
        }
      }
    })
    addAndRemoveBoxes(add, idsToRemove)
  }

  /**
    * Given the boxes to add and remove, updates the "walletBoxes" accordingly.
    * @param add - boxes to add in the form: public key -> {id1 -> box}, {id2 -> box2}
    * @param remove - boxes to remove in the form: {(public key, {id1, id2}), (publicKey2, {id3, id4})}
    */
  def addAndRemoveBoxes (add: MMap[String, MMap[String, Json]], remove: List[String]): Unit = {
    val idsToBoxes: MMap[String, Json] = walletBoxes.flatMap(box => box._2)
    remove.foreach {id =>
      idsToBoxes.get(id) match {
        case Some(box) =>
          val pubKey = (box \\ "proposition").head.toString()
          walletBoxes.get(pubKey).map(boxes => boxes.remove(id))
        case None =>
      }
    }
    /*remove.foreach { case (publicKey, ids) =>
      walletBoxes.get(publicKey).map(boxes => ids.foreach(boxes.remove))
    }*/
    add.foreach { case (publicKey, newBoxes) =>
      walletBoxes.get(publicKey).map(boxes => newBoxes.foreach(box => boxes.put(box._1, box._2)))
    }
  }

  override def receive: Receive = {
    case GjallarhornStarted => bifrostActorRef ! s"Remote wallet actor initialized. My public keys are: ${walletBoxes.keySet}"

    case msg: String => msgHandling(msg)

    case GetNewBlock => sender ! newestTransactions

    case UpdateWallet(updatedBoxes) => sender ! parseAndUpdate(updatedBoxes)

    /*case UpdateWallet(add, remove) => {
      remove.foreach { case (publicKey, ids) =>
        walletBoxes.get(publicKey).map(boxes => ids.foreach(boxes.remove))
      }
      add.foreach { case (publicKey, newBoxes) =>
        walletBoxes.get(publicKey).map(boxes => newBoxes.foreach(box => boxes.put(box._1, box._2)))
      }
    }*/
    case GjallarhornStopped =>
      val response: String = Await.result((bifrostActorRef ? "Remote wallet actor stopped").mapTo[String], 10.seconds)
      sender ! response

    case GetWallet => sender ! walletBoxes

    case IsConnected => sender ! connectedToBifrost

  }
}

object WalletManager {

  /**
    * Given the updated boxes, updates the walletboxes and returns the updated walletboxes
    * @param updatedBoxes - the current balances from Bifrost
    */
  case class UpdateWallet(updatedBoxes: Json)

  case object GjallarhornStarted

  case object GjallarhornStopped

  case object GetNewBlock

  case class NewBlock(block: String)

  case object GetWallet

  case object IsConnected

}
