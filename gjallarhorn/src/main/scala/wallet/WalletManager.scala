package wallet

import akka.actor.{Actor, ActorRef}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import crypto.{Address, Transaction}
import io.circe.{Json, ParsingFailure, parser}
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import utils.Logging
import cats.syntax.show._
import keymanager.KeyManager.GetOpenKeyfiles

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

/**
  * The WalletManager manages the communication between Bifrost and Gjallarhorn
  * Mainly, the WalletManager receives new blocks from Bifrost in order to updates its wallet boxes.
  * @param bifrostActorRef: the actor ref for Bifrost's WalletConnectionHandler.
  */
class WalletManager(bifrostActorRef: ActorRef)
                   ( implicit ec: ExecutionContext ) extends Actor with Logging {

  import WalletManager._

  implicit val timeout: Timeout = 10.seconds

  var connectedToBifrost: Boolean = false
  private var keyManagerRef: Option[ActorRef] = None


  //Represents the wallet boxes: as a mapping of publicKeys to a map of its id's mapped to walletBox.
  //Ex: publicKey1 -> {id1 -> walletBox1, id2 -> walletBox2, ...}, publicKey2 -> {},...
  var walletBoxes: MMap[String, MMap[String, Json]] = MMap.empty

  var newestTransactions: Option[String] = None

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.debug(s"WalletManagerActor: preRestart ${reason.getMessage}")
    log.debug(s"WalletManagerActor message: ${message.getOrElse("")}")
    super.preRestart(reason, message)
  }

  def initializeWalletBoxes(addresses: Set[Address]): Unit = {
    val returnVal: MMap[String, MMap[String, Json]] = MMap.empty
    addresses.map(key =>
      returnVal.put(key.toString, MMap.empty)
    )
    walletBoxes = returnVal
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT
  override def receive: Receive =
    initialization orElse
      nonsense

  private def active: Receive =
    operational orElse
      walletManagement orElse
      nonsense

  private def initialization: Receive = {
    case GjallarhornStarted =>
      bifrostActorRef ! s"Remote wallet actor initialized."

    case GetNetwork =>
      val bifrostResp: Future[Any] =
        bifrostActorRef ? "Which network is bifrost running?"
      bifrostResp.pipeTo(sender())

    case KeyManagerReady(keyMngrRef) =>
      val publicKeys = Await.result((keyMngrRef ? GetOpenKeyfiles)
        .mapTo[Set[Address]], 10.seconds)
      println("public keys: " + publicKeys)
      keyManagerRef = Some(keyMngrRef)
      initializeWalletBoxes(publicKeys)
      if (publicKeys.nonEmpty) {
        val balances: Json = Await.result((bifrostActorRef ? s"My public keys are: $publicKeys")
          .mapTo[String].map(_.asJson), 10.seconds)
        parseAndUpdate(parseResponse(balances))
      }else{
        log.debug(s"${Console.RED}You do not have any keys in your wallet! ${Console.RESET}")
      }
      context become active

    case msg: String => msgHandling(msg)
  }

  private def operational: Receive = {
    case msg: String => msgHandling(msg)

    case GetNewBlock => sender ! newestTransactions

    case GjallarhornStopped =>
      val response: String = Await.result((bifrostActorRef ? "Remote wallet actor stopped").mapTo[String], 10.seconds)
      sender ! response
  }

  private def walletManagement: Receive = {
    case UpdateWallet(updatedBoxes) => sender ! parseAndUpdate(updatedBoxes)
    case GetWallet => sender ! walletBoxes
    case NewKey(address) =>
      walletBoxes.put(address.toString, MMap.empty)
      bifrostActorRef ! s"New key: $address"
  }


  private def nonsense: Receive = {
    case nonsense: Any =>
      log.warn(s"Got unexpected input $nonsense from ${sender()}")
  }

  /**
    * Handles messages received from Bifrost
    * @param msg - handles "received new wallet" or "new block added" messages.
    */
  def msgHandling(msg: String): Unit = {
    if (msg.contains("received new wallet from:")) {
      log.info("Bifrost " + msg)
    }
    if (msg.contains("new block added")) {
      newBlock(msg)
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  //------------------------------------------------------------------------------------
  //Methods for parsing balance response - UpdateWallet

  def parseResponse(result: Json): Json = {
    val resultString = result.toString().replace("\\", "").replace("\"{", "{")
      .replace("}\"", "}")
    parse(resultString) match {
      case Left(f) => throw f
      case Right(res: Json) => res
    }
  }

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
        val assets: List[Json] = boxes.head \\ "3"
        val poly: List[Json] = boxes.head \\ "2"
        val arbit: List[Json] = boxes.head \\ "1"
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
    //log.info(s"Wallet Manager received new block with transactions: $blockTxs")
    parseTxsFromBlock(blockTxs)
    newestTransactions = Some(blockTxs)
  }

  def parseTxsFromBlock(txs: String): Unit = {
    parser.decode[List[Transaction]](txs) match {
      case Right(transactions) =>
        val add: MMap[String, MMap[String, Json]] = MMap.empty
        var idsToRemove: List[String] = List.empty
        transactions.foreach(tx => {
          tx.newBoxes.foreach(newBox => {
            val publicKey: String = newBox.evidence.toString
            var idToBox: MMap[String, Json] = MMap.empty
            add.get(publicKey) match {
              case Some(boxesMap) => idToBox = boxesMap
              case None => idToBox = MMap.empty
            }
            val id: String = newBox.id
            idToBox.put(id, newBox.asJson)
            add.put(publicKey, idToBox)
          })
          idsToRemove = tx.boxesToRemove match {
            case Some(seq) => seq.toList
            case None => List.empty
          }
        })
        addAndRemoveBoxes(add, idsToRemove)
      case Left(ex) => println(s"Not able to parse transactions: ${ex.show}")
    }

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
    add.foreach { case (publicKey, newBoxes) =>
      walletBoxes.get(publicKey).map(boxes => newBoxes.foreach(box => boxes.put(box._1, box._2)))
    }
  }

}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object WalletManager {

  val actorName = "WalletManager"

  /**
    * Given the updated boxes, updates the walletboxes and returns the updated walletboxes
    * @param updatedBoxes - the current balances from Bifrost
    */
  case class UpdateWallet(updatedBoxes: Json)

  case object GjallarhornStarted

  case object GetNetwork

  case object GjallarhornStopped

  case object GetNewBlock

  case class NewBlock(block: String)

  case object GetWallet

  case class KeyManagerReady(keyManagerRef: ActorRef)

  case class NewKey(address: Address)

}
