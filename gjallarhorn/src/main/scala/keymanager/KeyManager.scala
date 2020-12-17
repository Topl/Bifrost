package keymanager

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import crypto.{Address, KeyfileCurve25519, PrivateKeyCurve25519}
import io.circe.Json
import io.circe.syntax._
import scorex.util.encode.Base58
import settings.NetworkType
import utils.Logging

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}


class KeyManager(keyFileDir: String) extends Actor with Logging {

  import KeyManager._

  private var keyRing: Keys[PrivateKeyCurve25519, KeyfileCurve25519] =
    Keys(keyFileDir, KeyfileCurve25519)(PrivateKeyCurve25519.secretGenerator, networkPrefix = networkPrefix)

  override def receive: Receive = {
    case GenerateKeyFile(password, seedOpt) => sender ! keyRing.generateKeyFile(password, seedOpt)

    case ImportKeyfile(password: String, mnemonic: String, lang: String) =>
      sender ! keyRing.importPhrase(password, mnemonic, lang)

    case UnlockKeyFile(pubKeyString, password) => sender ! keyRing.unlockKeyFile(pubKeyString, password)

    case LockKeyFile(pubKeyString) => sender ! keyRing.lockKeyFile(pubKeyString)

    case GetOpenKeyfiles => sender ! keyRing.addresses

    case SignTx(tx: Json, keys: List[String], msg: Json) =>
      val signaturesMap = keys.map(keyString => {
        Base58.decode(msg.asString.get) match {
          case Success(msgToSign) =>
            keyRing.signWithAddress(Address(keyString), msgToSign) match {
              case Success(signedTx) =>
                val sig = signedTx.asJson
                keyRing.lookupPublicKey(Address(keyString)) match {
                  case Success(pubKey) => pubKey -> sig
                  case Failure(exception) => throw exception
                }
              case Failure(exception) => throw exception
            }
          case Failure(exception) => throw exception
        }
      }).toMap.asJson
      val newTx = tx.deepMerge(Map(
        "signatures" -> signaturesMap
      ).asJson)
      sender ! Map("tx"-> newTx).asJson

    case ChangeNetwork(networkName: String) =>
      NetworkType.fromString(networkName) match {
        case Some(network) =>
          //lock all keyfiles on current network
          keyRing.addresses.foreach(addr =>
            keyRing.lockKeyFile(addr.toString))

          //change network and initialize keyRing with new network
          networkPrefix = network.netPrefix
          keyRing = Keys(keyFileDir, KeyfileCurve25519)(PrivateKeyCurve25519.secretGenerator,
            networkPrefix = networkPrefix)

          log.info(s"${Console.MAGENTA}Network changed to: ${network.verboseName} ${Console.RESET}")
          sender ! Map("newNetworkPrefix" -> networkPrefix).asJson
        case None => Map("error" -> s"The network name: $networkName was not a valid network type!").asJson
      }
  }
}

object KeyManager {
  case class GenerateKeyFile(password: String, seedOpt: Option[String])
  case class ImportKeyfile(password: String, mnemonic: String, lang: String)
  case class UnlockKeyFile(publicKeyString: String, password: String)
  case class LockKeyFile(publicKeyString: String)
  case object GetOpenKeyfiles
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