package co.topl.loadtesting

import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.{ActorRef, Scheduler}
import akka.util.Timeout
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}

import scala.collection.immutable.ListMap
import scala.concurrent.Future

package object transactions {

  def signMessage(address: Address, message: Array[Byte], keys: ActorRef[KeysActor.Command])(implicit
    timeout:               Timeout,
    scheduler:             Scheduler
  ): Future[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]] =
    keys.ask(KeysActor.SignMessage(address, message, _))
}
