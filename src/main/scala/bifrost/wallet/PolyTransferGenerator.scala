package bifrost.wallet

import akka.actor.{Actor, ActorRef}
import bifrost.transaction.{PolyTransfer, PolyTransfer$}
import scorex.core.LocalInterface.LocallyGeneratedTransaction
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Random, Success, Try}

/**
  * Generator of PolyTransfer inside a wallet
  */
class PolyTransferGenerator(viewHolderRef: ActorRef) extends Actor {

  import PolyTransferGenerator._

  override def receive: Receive = {
    case StartGeneration(duration) =>
      context.system.scheduler.schedule(duration, duration, viewHolderRef, GetCurrentView)

    case CurrentView(_, state: BifrostState, wallet: BWallet, _) =>
      generate(wallet) match {
        case Success(tx) =>
          println(s"Local tx with with ${tx.from.size} inputs, ${tx.to.size} outputs")
          viewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], PolyTransfer](tx)
        case Failure(e) =>
          e.printStackTrace()
      }
  }

  def generate(wallet: BWallet): Try[PolyTransfer] = generateStatic(wallet)
}

object PolyTransferGenerator {

  case class StartGeneration(delay: FiniteDuration)

  def generateStatic(wallet: BWallet): Try[PolyTransfer] = {
    println(s"Wallet's public keys: ${wallet.publicKeys}")
    val pubkeys: Seq[PublicKey25519Proposition] = wallet.publicKeys.flatMap {
      case pkp: PublicKey25519Proposition => Some(pkp)
      case _ => None
    }.toSeq
    //todo multiple recipients
    val recipient = pubkeys(Random.nextInt(pubkeys.size))
    PolyTransfer.create(wallet, recipient, Random.nextInt(100), Random.nextInt(100))
  }
}
