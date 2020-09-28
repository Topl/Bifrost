package wallet

import akka.actor.Actor
import io.circe.Json
import scala.collection.mutable.{Map => MMap}

class WalletManager extends Actor {

  import WalletManager._

  var walletBoxes: MMap[String, MMap[String, Json]] = MMap.empty

  override def receive: Receive = {

    case UpdateWallet(add, remove) =>
      remove.foreach { case(publicKey, ids) =>
        walletBoxes.get(publicKey).map(boxes => ids.foreach(boxes.remove))
      }
      add.foreach { case(publicKey, newBoxes) =>
        walletBoxes.get(publicKey).map(boxes => newBoxes.foreach(box => boxes.put(box._1, box._2)))
      }
  }
}

object WalletManager {
  case class UpdateWallet(add: Map[String, Map[String, Json]], remove: List[(String, List[String])])
}
