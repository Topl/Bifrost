package keymanager

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scorex.crypto.hash.Blake2b256

import keymanager.KeyFile._

//Instantiate Necessary Actor System and Execution Context
implicit val actorsystem = ActorSystem("KeyManagerActorSys") //check name arbitrage?
implicit val ec: ExecutionContext = actorsystem.dispatcher

//DOMAIN: KeyManager Actor
object KeyManagerActor {
  case class GenerateKeyFile(password: String, seed: Array[Byte] = Blake2b256(uuid), defaultKeyDir: String)
  case class UnlockKeyFile(publicKeyString: String, password: String)
  case class LockKeyFile(publicKeyString: String, password: String)
}

//DOMAIN: For custom instantiation of KeyManager Actor
object KeyManagerActorRef {
  def apply(var secrets: Set[PrivateKey25519], defaultKeyDir: String)(implicit actorsystem: ActorSystem, ec: ExecutionContext): ActorRef = actorsystem.actorOf(props(secrets, defaultKeyDir))
  def props(var secrets: Set[PrivateKey25519], defaultKeyDir: String)(implicit ec: ExecutionContext): Props = Props(new KeyManagerActor(secrets, defaultKeyDir))
}

class KeyManager extends Actor {

  import KeyManager._

  //val keyManager = Keys(Set.empty, "")
  //VAR keyManager to be assigned said attribute from instantiation, not left empty as default

  //Overload messaging, stateful necessary
  override def receive: Receive = {
    case GenerateKeyFile(password, seed, defaultKeyDir) =>
      KeyFile.apply(password, seed, defaultKeyDir)

    case UnlockKeyFile(pubKeyString, password) => ???

    case LockKeyFile(pubKeyString, password) => ???
  }
}