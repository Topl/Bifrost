package bifrost.wallet

import akka.actor.{Actor, ActorRef}
import bifrost.LocalInterface.LocallyGeneratedTransaction
import bifrost.tokenBoxRegistry.TokenBoxRegistry
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.transaction.bifrostTransaction.PolyTransfer
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.state.PrivateKey25519
import io.iohk.iodb.ByteArrayWrapper
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Random, Success, Try}

/**
  * Generator of PolyTransfer inside a wallet
  */
class PolyTransferGenerator(viewHolderRef: ActorRef) extends Actor {

  import bifrost.wallet.PolyTransferGenerator._

  override def receive: Receive = {
    case StartGeneration(duration) =>
      context.system.scheduler.schedule(duration, duration, viewHolderRef, GetCurrentView)

    case CurrentView(_, state: BifrostState, wallet: BWallet, _) =>
      generate(wallet, state.tbr, state.nodeKeys) match {
        case Success(tx) =>
          viewHolderRef ! LocallyGeneratedTransaction[ProofOfKnowledgeProposition[PrivateKey25519], PolyTransfer](tx)
        case Failure(e) =>
          e.printStackTrace()
      }
  }

  def generate(wallet: BWallet, tbr: TokenBoxRegistry, nodeKeys: Set[ByteArrayWrapper]): Try[PolyTransfer] = generateStatic(wallet, tbr, nodeKeys)
}

object PolyTransferGenerator {

  case class StartGeneration(delay: FiniteDuration)

  def generateStatic(wallet: BWallet, tbr: TokenBoxRegistry, nodeKeys: Set[ByteArrayWrapper]): Try[PolyTransfer] = {

    val pubkeys: IndexedSeq[PublicKey25519Proposition] = wallet
      .publicKeys
      .flatMap {
        case pkp: PublicKey25519Proposition => Some(pkp)
        case _ => None
      }.toIndexedSeq

    pubkeys.foreach(key => if(!nodeKeys.contains(ByteArrayWrapper(key.pubKeyBytes))) throw new Exception("Node not set to watch for specified public key"))

    val recipient = pubkeys(Random.nextInt(pubkeys.size))
    PolyTransfer.create(tbr, wallet, IndexedSeq((recipient, Random.nextInt(100))), pubkeys, Random.nextInt(100), "")
  }
}
