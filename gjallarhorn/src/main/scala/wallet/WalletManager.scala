package wallet

import akka.actor.Actor
import crypto.{PrivateKey25519, ProofOfKnowledgeProposition}
import io.circe.Json

import scala.collection.mutable.{Map => MMap}


class WalletManager(publicKeys: Set[ProofOfKnowledgeProposition[PrivateKey25519]]) extends Actor {

  import WalletManager._

  var walletBoxes: MMap[String, MMap[String, Json]] = {
    val returnVal: MMap[String, MMap[String, Json]] = MMap.empty
    publicKeys.map(key =>
      returnVal.put(key.toString, MMap.empty)
    )
    returnVal
  }


  override def receive: Receive = {

    case UpdateWallet(add, remove) => {
      remove.foreach { case (publicKey, ids) =>
        walletBoxes.get(publicKey).map(boxes => ids.foreach(boxes.remove))
      }
      add.foreach { case (publicKey, newBoxes) =>
        walletBoxes.get(publicKey).map(boxes => newBoxes.foreach(box => boxes.put(box._1, box._2)))
      }
    }

    case GetWalletBoxes() => {
      sender ! walletBoxes
    }
  }
}

object WalletManager {
  case class UpdateWallet(add: MMap[String, MMap[String, Json]], remove: List[(String, List[String])])
  case class GetWalletBoxes()
}
