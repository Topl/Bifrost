package keymanager

import akka.actor.Actor

//DOMAIN: KeyManager Actor
object KeyManagerActor {
  case class lockKeyFile(publicKeyString: String, password: String)
  case class unlockKeyFile(publicKeyString: String, password: String)
}

class KeyManager extends Actor {

  override def receive: Receive = ???
}
