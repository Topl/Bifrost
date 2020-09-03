package keymanager

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.concurrent.ExecutionContext


class KeyManager(keyDir: String) extends Actor {

  import KeyManager._

  val keyManager: Keys = Keys(Set.empty, keyDir)

  //Overload messaging, stateful necessary
  // is the idea here to have another keys actor??? So that the keyManager requests using ask (?), then key tells (!) sender?
  override def receive: Receive = {
    case GenerateKeyFile(password, seed, defaultKeyDir) =>
      sender() ! KeyFile.apply(password, seed, defaultKeyDir)

    case UnlockKeyFile(pubKeyString, password) => keyManager.unlockKeyFile(pubKeyString, password)

    case LockKeyFile(pubKeyString, password) => keyManager.lockKeyFile(pubKeyString, password)

    case getOpenKeyfiles() =>
      sender ! keyManager.listOpenKeyFiles
  }
}

object KeyManager {
  case class GenerateKeyFile(password: String, seed: Array[Byte], defaultKeyDir: String)
  case class UnlockKeyFile(publicKeyString: String, password: String)
  case class LockKeyFile(publicKeyString: String, password: String)
  case class getOpenKeyfiles()
}

object KeyManagerRef {

  def props(keyDir: String)(implicit ec: ExecutionContext): Props = {
    Props(new KeyManager(keyDir))
  }

  def apply(name: String, keyDir: String)(implicit system: ActorSystem, ec: ExecutionContext): ActorRef = {
    system.actorOf(Props(new KeyManager(keyDir)))
  }
}