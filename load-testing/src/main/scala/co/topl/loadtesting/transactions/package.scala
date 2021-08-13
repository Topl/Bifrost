package co.topl.loadtesting

import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.{ActorRef, Scheduler}
import akka.util.Timeout
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}

import scala.collection.immutable.ListMap
import scala.concurrent.Future

package object transactions {

  /**
   * Requests a signature for a given message with a given address from the actor holding the key ring.
   * @param address the address to sign with
   * @param message the message to sign
   * @param keys the actor which has the ability to perform signing on the behalf of other actors
   * @param timeout the amount of time to wait for the keys actor to sign
   * @param scheduler the parent context's scheduler
   * @return a proposition map with signatures
   */
  def signMessage(address: Address, message: Array[Byte], keys: ActorRef[KeysActor.Command])(implicit
    timeout:               Timeout,
    scheduler:             Scheduler
  ): Future[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]] =
    keys.ask(KeysActor.SignMessage(address, message, _))
}
