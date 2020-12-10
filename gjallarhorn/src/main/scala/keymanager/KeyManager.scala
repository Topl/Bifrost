package keymanager

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import crypto.AddressEncoder.NetworkPrefix
import crypto.{Address, KeyfileCurve25519, PrivateKeyCurve25519}
import io.circe.Json
import io.circe.syntax._
import scorex.util.encode.Base58

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}


class KeyManager(keys: Keys[PrivateKeyCurve25519, KeyfileCurve25519])(implicit np: NetworkPrefix) extends Actor {

  import KeyManager._

  val keyManager: Keys[PrivateKeyCurve25519, KeyfileCurve25519] = keys

  override def receive: Receive = {
    case GenerateKeyFile(password, seedOpt) => sender ! keyManager.generateKeyFile(password, seedOpt)

    case ImportKeyfile(password: String, mnemonic: String, lang: String) => sender ! keyManager.importPhrase(password, mnemonic, lang)

    case UnlockKeyFile(pubKeyString, password) => sender ! keyManager.unlockKeyFile(pubKeyString, password)

    case LockKeyFile(pubKeyString, password) => sender ! keyManager.lockKeyFile(pubKeyString, password)

    case GetOpenKeyfiles => sender ! keyManager.addresses

    case SignTx(tx: Json, keys: List[String], msg: Json) =>
      val signaturesMap = keys.map(keyString => {
        Base58.decode(msg.asString.get) match {
          case Success(msgToSign) =>
            keyManager.signWithAddress(Address(keyString), msgToSign) match {
              case Success(signedTx) => {
                val sig = signedTx.asJson
                keyManager.lookupPublicKey(Address(keyString)) match {
                  case Success(pubKey) => pubKey -> sig
                  case Failure(exception) => throw exception
                }
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
  }
}

object KeyManager {
  case class GenerateKeyFile(password: String, seedOpt: Option[String])
  case class ImportKeyfile(password: String, mnemonic: String, lang: String)
  case class UnlockKeyFile(publicKeyString: String, password: String)
  case class LockKeyFile(publicKeyString: String, password: String)
  case object GetOpenKeyfiles
  case class SignTx(transaction: Json, signingKeys: List[String], messageToSign: Json)
}

object KeyManagerRef {

  def props(keys: Keys[PrivateKeyCurve25519, KeyfileCurve25519])
           (implicit ec: ExecutionContext, np: NetworkPrefix): Props = {
    Props(new KeyManager(keys))
  }

  def apply(name: String, keys: Keys[PrivateKeyCurve25519, KeyfileCurve25519])
           (implicit system: ActorSystem, ec: ExecutionContext, np: NetworkPrefix): ActorRef = {
    system.actorOf(Props(new KeyManager(keys)), name = name)
  }
}