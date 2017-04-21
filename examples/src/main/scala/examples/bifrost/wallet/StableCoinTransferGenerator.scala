package examples.bifrost.wallet

import akka.actor.{Actor, ActorRef}
import examples.bifrost.transaction.StableCoinTransfer
import examples.hybrid.state.{HBoxStoredState, SimpleBoxTransaction}
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import examples.bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Random, Success, Try}

/**
  * Generator of StableCoinTransfer inside a wallet
  */
class StableCoinTransferGenerator(viewHolderRef: ActorRef) extends Actor {

  import StableCoinTransferGenerator._

  override def receive: Receive = {
    case StartGeneration(duration) =>
      context.system.scheduler.schedule(duration, duration, viewHolderRef, GetCurrentView)

    case CurrentView(_, state: HBoxStoredState, wallet: BWallet, _) =>
      generate(wallet) match {
        case Success(tx) =>
          println(s"Local tx with with ${tx.from.size} inputs, ${tx.to.size} outputs")
          viewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], StableCoinTransfer](tx)
        case Failure(e) =>
          e.printStackTrace()
      }
  }

  def generate(wallet: BWallet): Try[StableCoinTransfer] = {
    val pubkeys: Seq[PublicKey25519Proposition] = wallet.publicKeys.flatMap {
      case pkp: PublicKey25519Proposition => Some(pkp)
      case _ => None
    }.toSeq
    //todo multiple recipients
    val recipient = pubkeys(Random.nextInt(pubkeys.size))
    StableCoinTransfer.create(wallet, recipient, Random.nextInt(100), Random.nextInt(100))
  }
}

object StableCoinTransferGenerator {

  case class StartGeneration(delay: FiniteDuration)

}
