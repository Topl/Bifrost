package bifrost.api.http

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericWalletBox
import bifrost.state.BifrostState
import bifrost.transaction.{ArbitTransfer, PolyTransfer}
import bifrost.transaction.box.{ArbitBox, BifrostBox, PolyBox}
import bifrost.wallet.BWallet
import io.circe.Json
import io.circe.parser.parse
import io.circe.parser._
import io.circe.syntax._
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import scorex.core.api.http.{ApiException, SuccessApiResponse}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
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
  override val route: Route = pathPrefix("walletrpc") { walletRoute }

  //noinspection ScalaStyle
  def walletRoute: Route = path("") { entity(as[String]) { body =>
    withAuth {
      postJsonRoute {
        viewAsync().map { view =>
          var reqId = ""
          parse(body) match {
            case Left(failure) => ApiException(failure.getCause)
            case Right(json) =>
              val futureResponse: Try[Future[Json]] = Try {
                val id = (json \\ "id").head.asString.get
                reqId = id
                require((json \\ "jsonrpc").head.asString.get == "2.0")
                val params = (json \\ "params").head.asArray.get
                require(params.size <= 5, s"size of params is ${params.size}")

                (json \\ "method").head.asString.get match {

                  case "transferPolys" => transferPolys(params.head, id)
                  case "transferArbits" => transferArbits(params.head, id)
                  case "balances" => balances(params.head, id)
                  case "unlockKeyfile" => unlockKeyfile(params.head, id)
                  case "lockKeyfile" => lockKeyfile(params.head, id)
                  case "generateKeyfile" => generateKeyfile(params.head, id)
                  case "listOpenKeyfiles" => listOpenKeyfiles(params.head, id)
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
  }}

  private def transferPolys(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault

      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)

      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }
      val publicKeysToSendFrom: Vector[String] = (params \\ "publicKeyToSendFrom").headOption match {
        case Some(keys) => keys.asArray.get.map(k => k.asString.get)
        case None => Vector()
      }
      val publicKeyToSendChangeTo: String = (params \\ "publicKeyToSendChangeTo").headOption match {
        case Some(key) => key.asString.get
        case None => if (publicKeysToSendFrom.nonEmpty) publicKeysToSendFrom.head else ""
      }

      // Call to BifrostTX to create TX
      val tx = PolyTransfer.create(wallet, IndexedSeq((recipient, amount)), fee, data, publicKeysToSendFrom, publicKeyToSendChangeTo).get

      // Update nodeView with new TX
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], PolyTransfer](tx)
      tx.json
    }
  }

  private def transferArbits(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault

      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)

      // Optional API parameters
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }
      val publicKeysToSendFrom: Vector[String] = (params \\ "publicKeyToSendFrom").headOption match {
        case Some(keys) => keys.asArray.get.map(k => k.asString.get)
        case None => Vector()
      }
      // Look for a specified change address, if not present then try to choose from the list of specified send keys, if no send keys let the wallet decide
      val publicKeyToSendChangeTo: String = (params \\ "publicKeyToSendChangeTo").headOption match {
        case Some(key) => key.asString.get
        case None => if (publicKeysToSendFrom.nonEmpty) publicKeysToSendFrom.head else ""
      }

      // Call to BifrostTX to create TX
      val tx = ArbitTransfer.create(wallet, IndexedSeq((recipient, amount)), fee, data, publicKeysToSendFrom, publicKeyToSendChangeTo).get

      // Update nodeView with new TX
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], ArbitTransfer](tx)
      tx.json
    }
  }

  private def balances(params: Json, id: String): Future[Json] = {
      viewAsync().map { view =>
        val wallet = view.vault

        // Optionally specify the publickey to get balances for. If empty string or not specified return all boxes
        val boxes: Seq[GenericWalletBox[Any, wallet.PI, BifrostBox]] = (params \\ "publicKey").headOption match {
          case Some(key) => if(key.asString.get != "") wallet.boxesByKey( key.asString.get ) else wallet.boxes()
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

//  private def balancesByKey(params: Json, id: String): Future[Json] = {
//    viewAsync().map { view =>
//      val wallet = view.vault
//      val publicKey: String = (params \\ "publicKey").head.asString.get
//
//      val boxes = wallet.boxesByKey(publicKey)
//
//      Map("polyBalance" -> boxes.flatMap(_.box match {
//        case pb: PolyBox => Some(pb.value)
//        case _ => None
//      }).sum.toString.asJson,
//        "arbitBalance" -> boxes.flatMap(_.box match {
//          case ab: ArbitBox => Some(ab.value)
//          case _ => None
//        }).sum.toString.asJson,
//        "publicKeys" -> wallet.publicKeys.flatMap(_ match {
//          case pkp: PublicKey25519Proposition => Some(Base58.encode(pkp.pubKeyBytes))
//          case _ => None
//        }).asJson,
//        "boxes" -> boxes.map(_.box.json).asJson
//      ).asJson
//    }
//  }

//  private def transfer(params: Json, id: String): Future[Json] = {
//    viewAsync().map { view =>
//      val wallet = view.vault
//      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
//      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
//      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
//      val data: String = (params \\ "data").head.asString.getOrElse("")
//      val publicKeysToSendFrom: Vector[String] = (params \\ "publicKeysToSendFrom").headOption match {
//        case Some(keys: Json) => keys.asArray.get.map(k => k.asString.get)
//        case None => Vector()
//      }
//      println(s">>>>>>> publicKeysToSendFrom: ${publicKeysToSendFrom}")
//      var publicKeyToSendChangeTo: String = ""
//      if(!(params \\ "publicKeyToSendChangeTo").isEmpty) {
//        publicKeyToSendChangeTo = (params \\ "publicKeyToSendChangeTo").head.asString.get
//      }
//      val tx = PolyTransfer.create(wallet, IndexedSeq((recipient, amount)), fee, data, publicKeysToSendFrom, publicKeyToSendChangeTo).get
//
//      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], PolyTransfer](tx)
//      tx.json
//    }
//  }

//  private def transferByKeys(params: Json, id: String): Future[Json] = {
//    viewAsync().map { view =>
//      val wallet = view.vault
//      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
//      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode((params \\ "recipient").head.asString.get).get)
//      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
//      val data: String = (params \\ "data").head.asString.get
//      var publicKeyToSendFrom: String = ""
////      if(!(params \\ "publicKeyToSendFrom").isEmpty) {
////        publicKeyToSendFrom = (params \\ "publicKeyToSendFrom").head.asString.get
////      }
//      val publicKeysToSendFrom = (params \\ "publicKeysToSendFrom").toSeq
//
//      val publicKeysToSendFrom2 = (params \\ "publicKeysToSendFrom").head.asInstanceOf[IndexedSeq[String]]
//
//
//      //      val publicKeysToSendFrom = (params \\ "publicKeysToSendFrom").children
//
//      println()
//      println(publicKeysToSendFrom)
//      println()
//
//      println(publicKeysToSendFrom2)
//
//      val tx = PolyTransfer.createByKey(wallet, IndexedSeq((recipient, amount)), fee, data, publicKeysToSendFrom).get
//      nodeViewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], PolyTransfer](tx)
//      tx.json
//    }
//  }
