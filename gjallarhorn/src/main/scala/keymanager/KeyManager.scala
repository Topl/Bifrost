package keymanager

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout
import attestation.{Address, PrivateKeyCurve25519}
import crypto.KeyfileCurve25519
import io.circe.Json
import io.circe.syntax._
import scorex.util.encode.Base58
import settings.NetworkType
import utils.Logging
import wallet.WalletManager
import wallet.WalletManager.NewKey

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._


class KeyManager(keyFileDir: String) extends Actor with Logging {

  import KeyManager._

  private var keyRing: Keys[PrivateKeyCurve25519, KeyfileCurve25519] =
    Keys(keyFileDir, KeyfileCurve25519)(PrivateKeyCurve25519.secretGenerator, networkPrefix = networkPrefix)

  implicit val timeout: Timeout = 10.seconds

  override def receive: Receive = {
    case GenerateKeyFile(password, seedOpt) =>
      shareNewKey(keyRing.generateKeyFile(password, seedOpt), sender())

    case ImportKeyfile(password: String, mnemonic: String, lang: String) =>
      shareNewKey(keyRing.importPhrase(password, mnemonic, lang), sender())

    case UnlockKeyFile(addressString, password) => sender ! keyRing.unlockKeyFile(addressString, password)

    case LockKeyFile(addressString) => sender ! keyRing.lockKeyFile(addressString)

    case GetOpenKeyfiles => sender ! keyRing.addresses

    case GetAllKeyfiles => sender ! keyRing.listKeyFilesAndStatus

    case SignTx(tx: Json, keys: List[String], msg: Json) =>
      for {
        currentSignatures <- (tx \\ "signatures").head.as[Map[PrivateKeyCurve25519#PK, Json]]
      } yield {
        val newSignatures: Map[PrivateKeyCurve25519#PK, Json] = keys.map(keyString => {
          Base58.decode(msg.asString.get) match {
            case Success(msgToSign) =>
              keyRing.signWithAddress(Address(networkPrefix)(keyString), msgToSign) match {
                case Success(signedTx) =>
                  val sig = signedTx.asJson
                  keyRing.lookupPublicKey(Address(networkPrefix)(keyString)) match {
                    case Success(pubKey) => pubKey -> sig
                    case Failure(exception) => throw exception
                  }
                case Failure(exception) => throw exception
              }
            case Failure(exception) => throw exception
          }
        }).toMap
        val newTx = tx.deepMerge(Map(
          "signatures" -> (currentSignatures ++ newSignatures).asJson
        ).asJson)
        sender ! Map("tx" -> newTx).asJson
      }

    case ChangeNetwork(networkName: String) =>
      NetworkType.fromString(networkName) match {
        case Some(network) =>
          if (network.netPrefix != networkPrefix) {
            //lock all keyfiles on current network
            keyRing.addresses.foreach(addr =>
              keyRing.lockKeyFile(addr.toString))

            //change network and initialize keyRing with new network
            networkPrefix = network.netPrefix
            keyRing = Keys(keyFileDir, KeyfileCurve25519)(PrivateKeyCurve25519.secretGenerator,
              networkPrefix = networkPrefix)

            //TODO: unlock all keyfiles on new network

            log.info(s"${Console.MAGENTA}Network changed to: ${network.verboseName} ${Console.RESET}")
          }
          sender ! Map("newNetworkPrefix" -> networkPrefix).asJson
        case None => Map("error" -> s"The network name: $networkName was not a valid network type!").asJson
      }
  }

  private def shareNewKey(newAddress: Try[Address], sender: ActorRef) {
    newAddress match {
      case Success(addr) =>
        context.actorSelection("../" + WalletManager.actorName).resolveOne().onComplete {
          case Success(walletActor) => walletActor ! NewKey(addr)
          case Failure(ex) => log.info("offline mode")
        }
      case Failure(ex) => log.error("unable to generate key file!")
    }
    sender ! newAddress
  }
}

object KeyManager {
  case class GenerateKeyFile(password: String, seedOpt: Option[String])
  case class ImportKeyfile(password: String, mnemonic: String, lang: String)
  case class UnlockKeyFile(addressString: String, password: String)
  case class LockKeyFile(addressString: String)
  case object GetOpenKeyfiles
  case object GetAllKeyfiles
  case class SignTx(transaction: Json, signingKeys: List[String], messageToSign: Json)
  case class ChangeNetwork(networkName: String)
}

object KeyManagerRef {

  def props(keyFileDir: String)
           (implicit ec: ExecutionContext): Props = {
    Props(new KeyManager(keyFileDir))
  }

  def apply(name: String, keyFileDir: String)
           (implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(Props(new KeyManager(keyFileDir)), name = name)
  }
}