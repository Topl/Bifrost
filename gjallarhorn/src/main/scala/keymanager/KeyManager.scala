package keymanager

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import crypto.{PrivateKey25519Companion, PublicKey25519Proposition}
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext


class KeyManager(keyDir: String) extends Actor {

  import KeyManager._

  val keyManager: Keys = Keys(Set.empty, keyDir)

  //Overload messaging, stateful necessary
  // is the idea here to have another keys actor??? So that the keyManager requests using ask (?), then key tells (!) sender?
  override def receive: Receive = {
    case GenerateKeyFile(password) =>
      sender ! Base58.encode(KeyFile(password, defaultKeyDir = keyManager.defaultKeyDir).pubKeyBytes)

    case UnlockKeyFile(pubKeyString, password) => keyManager.unlockKeyFile(pubKeyString, password)

    case LockKeyFile(pubKeyString, password) => keyManager.lockKeyFile(pubKeyString, password)

    case GetOpenKeyfiles =>
      sender ! keyManager.listOpenKeyFiles

    case SignTx(tx, keys, msg) =>
      val sigs: List[(String, String)] = keys.map { pk =>
        val pubKey = PublicKey25519Proposition(Base58.decode(pk).get)
        val privKey = keyManager.secrets.find(sk => sk.publicKeyBytes sameElements pubKey.pubKeyBytes)

        privKey match {
          case Some(sk) => {
            val signature = Base58.encode(PrivateKey25519Companion.sign(sk, Base58.decode(msg.asString.get).get).signature)
            (pk, signature)
          }
          case None => throw new NoSuchElementException
        }
      }
      // not sure this is necessary, but seems like it? Updating the signatures field
      val newTx = tx.deepMerge(Map(
        "signatures" -> sigs.toMap.asJson
      ).asJson)
      sender ! Map("formattedTx"-> newTx).asJson
  }
}

object KeyManager {
  case class GenerateKeyFile(password: String)
  case class UnlockKeyFile(publicKeyString: String, password: String)
  case class LockKeyFile(publicKeyString: String, password: String)
  case object GetOpenKeyfiles
  case class SignTx(transaction: Json, signingKeys: List[String], messageToSign: Json)
}

object KeyManagerRef {

  def props(keyDir: String)(implicit ec: ExecutionContext): Props = {
    Props(new KeyManager(keyDir))
  }

  def apply(name: String, keyDir: String)(implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(Props(new KeyManager(keyDir)))
  }
}