package keymanager

import akka.actor.Actor
import keymanager.KeyFile.uuid
import scorex.crypto.hash.Blake2b256

object KeyManager {
  case class GenerateKeyFile(password: String, seed: Array[Byte] = Blake2b256(uuid), defaultKeyDir: String)
  case class UnlockKeyFile(publicKeyString: String, password: String)
  case class LockKeyFile(publicKeyString: String, password: String)
}

//DOMAIN: KeyManager Actor
object KeyManagerActor {
  case class lockKeyFile(publicKeyString: String, password: String)
  case class unlockKeyFile(publicKeyString: String, password: String)
}

class KeyManager extends Actor {

  import KeyManager._

  val keyManager = Keys(Set.empty, "")

  override def receive: Receive = {
    case GenerateKeyFile(password, seed, defaultKeyDir) => KeyFile.apply(password, seed, defaultKeyDir)
    case UnlockKeyFile(pubKeyString, password) => // not context.become bc we want stateful actors?
    case LockKeyFile(pubKeyString, password) =>
  }
}
