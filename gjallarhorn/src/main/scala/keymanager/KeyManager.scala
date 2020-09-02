package keymanager

import java.io.File

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.crypto.hash.Blake2b256

import scala.concurrent.ExecutionContext


class KeyManager(keyDir: String) extends Actor {

  import KeyManager._

  val keyManager: Keys = Keys(Set.empty, keyDir)

  //Overload messaging, stateful necessary
  override def receive: Receive = {
    case GenerateKeyFile(password, seed, defaultKeyDir) =>
      KeyFile.apply(password, seed, defaultKeyDir)

    case UnlockKeyFile(pubKeyString, password) => ???

    case LockKeyFile(pubKeyString, password) => ???
  }
}

object KeyManager {
  case class GenerateKeyFile(password: String, seed: Array[Byte], defaultKeyDir: String)
  case class UnlockKeyFile(publicKeyString: String, password: String)
  case class LockKeyFile(publicKeyString: String, password: String)
}

object KeyManagerRef {

  def props(keyDir: String)(implicit ec: ExecutionContext): Props = {
    Props(new KeyManager(keyDir))
  }

  def apply(name: String, keyDir: String)(implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(Props(new KeyManager(keyDir)))
  }
}