package co.topl.loadtesting

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import akka.stream.Materializer
import co.topl.attestation.keyManagement._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.utils.NetworkType.NetworkPrefix

import scala.collection.immutable.ListMap
import scala.util.Try

/**
 * Contains all of the keys of currently active users.
 */
object KeysActor {

  sealed trait Command

  /**
   * Generates a given number of key pairs from the given seed.
   * Sends back the resulting addresses to the given `ActorRef[Try[Set[Address]]]` actorRef.
   * @param seed the seed to generate the addresses
   * @param num the number of addresses to generate
   * @param replyTo the actor to reply to with the set of addresses if generation was successful
   */
  case class GenerateKeyPairs(seed: String, num: Int, replyTo: ActorRef[Try[Set[Address]]]) extends Command

  /**
   * Signs a message on the behalf of an address.
   * @param address the address to sign for
   * @param message the message to sign
   * @param replyTo the actor to send the signed message to
   */
  case class SignMessage(
    address: Address,
    message: Array[Byte],
    replyTo: ActorRef[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]]
  ) extends Command

  /**
   * Instantiates a KeysActor behavior with the given `KeyRing`
   * @param k the key ring that this actor should hold
   * @param netPrefix the Bifrost network prefix
   * @param materializer a stream materializer
   * @return an actor behavior
   */
  private def withState(
    k: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519]
  )(implicit netPrefix: NetworkPrefix, materializer: Materializer): Behavior[Command] =
    Behaviors.receiveMessage {
      case GenerateKeyPairs(seed, num, replyTo) =>
        replyTo ! k.generateNewKeyPairs(num, Some(seed)).map(_.map(_.publicImage.address))
        withState(k)
      case SignMessage(addr, message, replyTo) =>
        replyTo ! k.generateAttestation(addr)(message)
        withState(k)
    }

  /**
   * Instantiates a new `Behavior` accepting a `KeysActor.Command`
   * @param networkPrefix the Bifrost network prefix
   * @return an actor behavior
   */
  def apply()(implicit networkPrefix: NetworkPrefix): Behavior[Command] =
    Behaviors.setup { context =>
      implicit val materializer: Materializer = Materializer(context)
      implicit val companion: KeyfileCompanion[PrivateKeyCurve25519, KeyfileCurve25519] = KeyfileCurve25519Companion
      withState(KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519](path = Some("./keyfiles")))
    }
}
