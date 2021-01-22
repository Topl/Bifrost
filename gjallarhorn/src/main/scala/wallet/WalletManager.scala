package wallet

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import attestation.Address
import crypto.{Box, Transaction}
import io.circe.{Json, parser}
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import utils.Logging
import cats.syntax.show._
import keymanager.KeyManager.{ChangeNetwork, GetAllKeyfiles}
import keymanager.networkPrefix
import modifier.BoxId
import settings.NetworkType

import scala.collection.mutable.{Map => MMap}
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * The WalletManager manages the communication between Bifrost and Gjallarhorn
  * Mainly, the WalletManager receives new blocks from Bifrost in order to updates its wallet boxes.
  * //@param bifrostActorRef: the actor ref for Bifrost's WalletConnectionHandler.
  */
class WalletManager(keyManagerRef: ActorRef)
                   ( implicit ec: ExecutionContext ) extends Actor with Logging {

  import WalletManager._

  implicit val timeout: Timeout = 10.seconds

  var connectedToBifrost: Boolean = false
  private var bifrostActorRef: Option[ActorRef] = None

  //Represents the wallet boxes: as a mapping of addresses to a map of its id's mapped to walletBox.
  //Ex: address1 -> {id1 -> walletBox1, id2 -> walletBox2, ...}, address2 -> {},...
  var walletBoxes: MMap[Address, MMap[BoxId, Box]] = initializeWalletBoxes()

  var newestTransactions: Option[String] = None

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.debug(s"WalletManagerActor: preRestart ${reason.getMessage}")
    log.debug(s"WalletManagerActor message: ${message.getOrElse("")}")
    super.preRestart(reason, message)
  }

  def initializeWalletBoxes(): MMap[Address, MMap[BoxId, Box]] = {
    val addresses = Await.result((keyManagerRef ? GetAllKeyfiles)
      .mapTo[Map[Address,String]].map(_.keySet), 10.seconds)

    val wallet: MMap[Address, MMap[BoxId, Box]] = MMap.empty
    addresses.map(key => wallet.put(key, MMap.empty))
    wallet
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

  // ----------- MESSAGE PROCESSING FUNCTIONS
  private def initialization: Receive = {
    case ConnectToBifrost(bifrostActor) =>
      setUpConnection(bifrostActor)
      context become active

    case GetWallet => sender ! walletBoxes

/*    case GetNetwork =>
      val bifrostResp: Future[Any] =
        bifrostActorRef ? "Which network is bifrost running?"
      bifrostResp.pipeTo(sender())*/

/*    /**
      * After setting up keyManager with correct network, grabs the open key files
      *
      * */
    case KeyManagerReady(keyMngrRef) =>
      val addresses: Set[Address] = Await.result((keyMngrRef ? GetAllKeyfiles)
        .mapTo[Map[Address,String]].map(_.keySet), 10.seconds)
      keyManagerRef = Some(keyMngrRef)
      initializeWalletBoxes(addresses)
      context become active*/

    case msg: String => msgHandling(msg)
  }

  private def operational: Receive = {
    case msg: String => msgHandling(msg)
    case GetNewBlock => sender ! newestTransactions
    case GjallarhornStopped =>
      bifrostActorRef match {
        case Some(actor) =>
          val response: String = Await.result((actor ? "Remote wallet actor stopped").mapTo[String], 10.seconds)
          sender ! response
          bifrostActorRef = None
        case None => log.warn("Already disconnected from Bifrost")
      }
  }

  private def walletManagement: Receive = {
    case UpdateWallet(updatedBoxes) => sender ! parseAndUpdate(updatedBoxes)
    case GetWallet => sender ! walletBoxes
    case GetConnection => sender ! bifrostActorRef
    case NewKey(address) =>
      walletBoxes.put(address, MMap.empty)
      bifrostActorRef match {
        case Some(actor) => actor ! s"New key: $address"
        case None =>
      }
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

  def setUpConnection(bifrost: ActorRef): Unit = {
    bifrostActorRef = Some(bifrost)

    //tell bifrost about this wallet
    bifrost ! s"Remote wallet actor initialized."

    //get network from bifrost and tell keyManager
    val networkResp: String = Await.result((bifrost ? "Which network is bifrost running?").mapTo[String], 10.seconds)
    val networkName = networkResp.split("Bifrost is running on").tail.head.replaceAll("\\s", "")
    (keyManagerRef ? ChangeNetwork(networkName)).onComplete {
      case Success(networkResponse: Json) => assert(NetworkType.fromString(networkName).get.netPrefix.toString ==
        (networkResponse \\ "newNetworkPrefix").head.asNumber.get.toString)
      case Success(_) | Failure(_) => throw new Exception ("was not able to change network")
    }

    //re-initialize walletboxes
    walletBoxes = initializeWalletBoxes()
    val addresses = walletBoxes.keySet

    //get balances from bifrost
    if (addresses.nonEmpty) {
      val balances: Json = Await.result((bifrost ? s"My addresses are: $addresses")
        .mapTo[String].map(_.asJson), 10.seconds)
      parseAndUpdate(parseResponse(balances))
    }else{
      log.debug(s"${Console.RED}You do not have any keys in your wallet! ${Console.RESET}")
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
  def parseBoxType (sameTypeBoxes: Json): MMap[BoxId, Box] = {
    parser.decode[List[Box]](sameTypeBoxes.toString()) match {
      case Right(boxes) => MMap(boxes.map(b => b.id -> b).toMap.toSeq: _*)
      case Left(ex) => throw new Exception(s"Unable to parse boxes from balance response: $ex")
    }
    /*
    val boxesMap: MMap[BoxId, Box] = MMap.empty
    val boxesArray: Array[String] = parseJsonList(sameTypeBoxes)
    boxesArray.foreach(asset => {
      val assetJson: Either[ParsingFailure, Json] = parse(asset)
      assetJson match {
        case Right(json) =>
          val id = (json \\ "id").head.asString.get
          boxesMap.put(id, json)
        case Left(e) => sys.error(s"Could not parse json: $e")
      }
    })
    boxesMap*/
  }

  /**
    * Given the balance response from Bifrost, parse the json and update wallet box
    * @param json - the balance response from Bifrost
    * @return - the updated walletBoxes
    */
  def parseAndUpdate(json: Json): MMap[Address, MMap[BoxId, Box]] = {
    val addresses: scala.collection.Set[Address] = walletBoxes.keySet
    addresses.foreach(addr => {
      val info: Json = (json \\ addr.toString).head
      var boxesMap: MMap[BoxId, Box] = MMap.empty
      val boxes = info \\ "Boxes"
      if (boxes.nonEmpty) {
        val assets: List[Json] = boxes.head \\ "AssetBox"
        val poly: List[Json] = boxes.head \\ "PolyBox"
        val arbit: List[Json] = boxes.head \\ "ArbitBox"
        if (assets.nonEmpty) {
          boxesMap = parseBoxType(assets.head)
        }
        if (poly.nonEmpty) {
          boxesMap = boxesMap ++ parseBoxType(poly.head)
        }
        if (arbit.nonEmpty) {
          boxesMap = boxesMap ++ parseBoxType(arbit.head)
        }
        walletBoxes(addr) = boxesMap}
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
        val add: MMap[Address, MMap[BoxId, Box]] = MMap.empty
        var idsToRemove: List[BoxId] = List.empty
        transactions.foreach(tx => {
          if (tx.newBoxes.nonEmpty) {
            log.info("Received transaction with boxes: " + tx.asJson)
          }
          tx.newBoxes.foreach(newBox => {
            val address: Address = Address(newBox.evidence)(networkPrefix)
            var idToBox: MMap[BoxId, Box] = MMap.empty
            add.get(address) match {
              case Some(boxesMap) => idToBox = boxesMap
              case None => idToBox = MMap.empty
            }
            idToBox.put(newBox.id, newBox)
            add.put(address, idToBox)
          })
          idsToRemove = tx.boxesToRemove match {
            case Some(seq) => seq.toList
            case None => List.empty
          }
        })
        addAndRemoveBoxes(add, idsToRemove)
      case Left(ex) => throw new Exception(s"Not able to parse transactions: ${ex.show}")
    }
  }

  /**
    * Given the boxes to add and remove, updates the "walletBoxes" accordingly.
    * @param add - boxes to add in the form: address -> {id1 -> box}, {id2 -> box2}
    * @param remove - list of ids for boxes to remove
    */
  def addAndRemoveBoxes (add: MMap[Address, MMap[BoxId, Box]], remove: List[BoxId]): Unit = {
    val idsToBoxes: MMap[BoxId, Box] = walletBoxes.flatMap(box => box._2)
    remove.foreach {id =>
      idsToBoxes.get(id) match {
        case Some(box) =>
          val address: Address = Address(box.evidence)(networkPrefix)
          walletBoxes.get(address).map(boxes => boxes.remove(id))
        case None => throw new Error(s"no box found with id: $id in $idsToBoxes")
      }
    }
    add.foreach { case (address, newBoxes) =>
      walletBoxes.get(address).map(boxes => newBoxes.foreach(box => boxes.put(box._1, box._2)))
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

  //case object GetNetwork

  case object GjallarhornStopped

  case object GetNewBlock

  case class NewBlock(block: String)

  case object GetWallet

  //case class KeyManagerReady(keyManagerRef: ActorRef)

  case class ConnectToBifrost(bifrostActor: ActorRef)

  case class NewKey(address: Address)

  case object GetConnection

}
