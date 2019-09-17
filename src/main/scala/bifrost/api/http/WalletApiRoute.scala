package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericWalletBox
import bifrost.state.BifrostState
import bifrost.transaction.box.{ArbitBox, BifrostBox, PolyBox}
import bifrost.wallet.BWallet
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import bifrost.LocalInterface.LocallyGeneratedTransaction
import bifrost.crypto.Bip39
import bifrost.settings.Settings
import bifrost.transaction.bifrostTransaction.{ArbitTransfer, PolyTransfer}
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.state.PrivateKey25519
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

case class WalletApiRoute(override val settings: Settings, nodeViewHolderRef: ActorRef)
                         (implicit val context: ActorRefFactory) extends ApiRouteWithView {
  type HIS = BifrostHistory
  type MS = BifrostState
  type VL = BWallet
  type MP = BifrostMemPool
  override val route: Route = pathPrefix("wallet") {
    walletRoute
  }

  //noinspection ScalaStyle
  def walletRoute: Route = path("") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          viewAsync().map { view =>
            var reqId = ""
            parse(body) match {
              case Left(failure) => ApiException(failure.getCause)
              case Right(request) =>
                val futureResponse: Try[Future[Json]] = Try {
                  val id = (request \\ "id").head.asString.get
                  reqId = id
                  require((request \\ "jsonrpc").head.asString.get == "2.0")
                  val params = (request \\ "params").head.asArray.get
                  //todo: why is there an enforcement on the size of params?
                  require(params.size <= 5, s"size of params is ${params.size}")

                  (request \\ "method").head.asString.get match {
                    case "transferPolys" => transferPolys(params.head, id)
                    case "transferPolysPrototype" => transferPolysPrototype(params.head, id)
                    case "transferArbits" => transferArbits(params.head, id)
                    case "transferArbitsPrototype" => transferArbitsPrototype(params.head, id)
                    case "balances" => balances(params.head, id)
                    case "unlockKeyfile" => unlockKeyfile(params.head, id)
                    case "lockKeyfile" => lockKeyfile(params.head, id)
                    case "generateKeyfile" => generateKeyfile(params.head, id)
                    case "listOpenKeyfiles" => listOpenKeyfiles(params.head, id)
                    case "importSeedPhrase" => importKeyfile(params.head, id)
                  }
                }
                futureResponse map {
                  response => Await.result(response, timeout.duration)
                }
                match {
                  case Success(resp) => BifrostSuccessResponse(resp, reqId)
                  case Failure(e) => BifrostErrorResponse(e, 500, reqId, verbose = settings.settingsJSON.getOrElse("verboseAPI", false.asJson).asBoolean.get)
                }
            }
          }
        }
      }
    }
  }

  private def transferPolys(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val sender: IndexedSeq[PublicKey25519Proposition] = (params \\ "sender").head.asArray.get.map(key => PublicKey25519Proposition(Base58.decode(key.asString.get).get)).toIndexedSeq
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }
      if(view.state.tbr == null) throw new Exception("TokenBoxRegistry not defined for node")
      //YT NOTE - if nodeKeys not defined in settings file then node watches for all keys in a state update
      if(view.state.nodeKeys != null)
        sender.foreach(key => if(!view.state.nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes))) throw new Exception("Node not set to watch for specified public key"))
      // Call to BifrostTX to create TX
      val tx = PolyTransfer.create(view.state.tbr, wallet, IndexedSeq((recipient, amount)), sender, fee, data).get
      // Update nodeView with new TX
      PolyTransfer.validate(tx) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], PolyTransfer](tx)
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  private def transferPolysPrototype(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val sender: IndexedSeq[PublicKey25519Proposition] = (params \\ "sender").head.asArray.get.map(key => PublicKey25519Proposition(Base58.decode(key.asString.get).get)).toIndexedSeq
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }

      if(view.state.tbr == null) throw new Exception("TokenBoxRegistry not defined for node")
      if(view.state.nodeKeys != null)
        sender.foreach(key => if(!view.state.nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes))) throw new Exception("Node not set to watch for specified public key"))
      val tx = PolyTransfer.createPrototype(view.state.tbr, IndexedSeq((recipient, amount)), sender, fee, data).get
      // Update nodeView with new TX
      PolyTransfer.validatePrototype(tx) match {
        case Success(_) =>
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  //YT NOTE - change is returned to first address from senders list
  private def transferArbits(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val sender: IndexedSeq[PublicKey25519Proposition] = (params \\ "sender").head.asArray.get.map(key => PublicKey25519Proposition(Base58.decode(key.asString.get).get)).toIndexedSeq
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }
      if(view.state.tbr == null) throw new Exception("TokenBoxRegistry not defined for node")
      if(view.state.nodeKeys != null)
        sender.foreach(key => if(!view.state.nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes))) throw new Exception("Node not set to watch for specified public key"))
      val tx = ArbitTransfer.create(view.state.tbr, wallet, IndexedSeq((recipient, amount)), sender, fee, data).get
      // Update nodeView with new TX
      println(tx.json)
      ArbitTransfer.validate(tx) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ArbitTransfer](tx)
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  private def transferArbitsPrototype(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val sender: IndexedSeq[PublicKey25519Proposition] = (params \\ "sender").head.asArray.get.map(key => PublicKey25519Proposition(Base58.decode(key.asString.get).get)).toIndexedSeq
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }

      if(view.state.tbr == null) throw new Exception("TokenBoxRegistry not defined for node")
      if(view.state.nodeKeys != null)
        sender.foreach(key => if(!view.state.nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes))) throw new Exception("Node not set to watch for specified public key"))
      val tx = ArbitTransfer.createPrototype(view.state.tbr, IndexedSeq((recipient, amount)), sender, fee, data).get
      // Update nodeView with new TX
      ArbitTransfer.validatePrototype(tx) match {
        case Success(_) =>
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  private def balances(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      // Optionally specify the publickey to get balances for. If empty string or not specified return all boxes
      val boxes: Seq[GenericWalletBox[Any, wallet.PI, BifrostBox]] = (params \\ "publicKey").headOption match {
        case Some(key) => if (key.asString.get != "") wallet.boxesByKey(key.asString.get) else wallet.boxes()
        case _ => wallet.boxes()
      }
      Map("polyBalance" -> boxes.flatMap(_.box match {
        case pb: PolyBox => Some(pb.value)
        case _ => None
      }).sum.toString.asJson,
        "arbitBalance" -> boxes.flatMap(_.box match {
          case ab: ArbitBox => Some(ab.value)
          case _ => None
        }).sum.toString.asJson,
        "publicKeys" -> wallet.publicKeys.flatMap(_ match {
          case pkp: PublicKey25519Proposition => Some(Base58.encode(pkp.pubKeyBytes))
          case _ => None
        }).asJson,
        "boxes" -> boxes.map(_.box.json).asJson
      ).asJson
    }
  }

  private def generateKeyfile(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val password: String = (params \\ "password").head.asString.get
      val pubKey = wallet.generateNewSecret(password)
      Map(
        "publicKey" -> Base58.encode(pubKey.pubKeyBytes).asJson
      ).asJson
    }
  }

  private def importKeyfile(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val password: String = (params \\ "password").head.asString.get
      val seedPhrase: String = (params \\ "seedPhrase").head.asString.get
      val seedPhraseLang: String = (params \\ "seedPhraseLang").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => "en"
      }
      val pt = Bip39.apply(seedPhraseLang)
      Map(
      pt.phraseCheckSum(seedPhrase) match {
        case false =>"error:" ->  "not a valid input phrase".asJson
        case true =>
        {
          val seed = pt.hexToUuid(pt.phraseToHex(seedPhrase))
          val pubKey = wallet.generateNewSecret(password, seed)
          "publicKey" -> Base58.encode(pubKey.pubKeyBytes).asJson
        }

      }
      ).asJson
    }
  }

  private def unlockKeyfile(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val publicKey: String = (params \\ "publicKey").head.asString.get
      val password: String = (params \\ "password").head.asString.get
      wallet.unlockKeyFile(publicKey, password)
      Map(
        publicKey -> "unlocked".asJson
      ).asJson
    }
  }

  private def lockKeyfile(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val publicKey: String = (params \\ "publicKey").head.asString.get
      val password: String = (params \\ "password").head.asString.get
      wallet.lockKeyFile(publicKey, password)
      Map(
        publicKey -> "locked".asJson
      ).asJson
    }
  }

  private def listOpenKeyfiles(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      wallet.secrets.flatMap(_ match {
        case pkp: PrivateKey25519 => Some(Base58.encode(pkp.publicKeyBytes))
        case _ => None
      }).asJson
    }
  }
}
