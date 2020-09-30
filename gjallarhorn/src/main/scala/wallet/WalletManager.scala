package wallet

import akka.actor.Actor
import io.circe.{Json, ParsingFailure}
import io.circe.parser.parse

import scala.collection.mutable.{Map => MMap}


class WalletManager(publicKeys: Set[String]) extends Actor {

  import WalletManager._

  var walletBoxes: MMap[String, MMap[String, Json]] = {
    val returnVal: MMap[String, MMap[String, Json]] = MMap.empty
    publicKeys.map(key =>
      returnVal.put(key.toString, MMap.empty)
    )
    returnVal
  }

  def parseUpdate(json: Json): MMap[String, MMap[String, Json]] = {
    val pubKeys: scala.collection.Set[String] = walletBoxes.keySet
    pubKeys.foreach(key => {
      val info: Json = (json \\ key).head
      val boxesMap: MMap[String, Json] = MMap.empty
      val boxes = (info \\ "Boxes")
      if (boxes.nonEmpty) {
        val boxesJson: Json = boxes.head
        val assets: List[Json] = boxesJson \\ "Asset"
        val poly: List[Json] = boxesJson \\ "Poly"
        val arbit: List[Json] = boxesJson \\ "Arbit"
        if (assets.nonEmpty) {
          val assetsJson: Json = assets.head
          var assetsArray: Array[String] = assetsJson.toString().trim.stripPrefix("[").stripSuffix("]").
            split("},")
          assetsArray = assetsArray.map(asset => {
            if (assetsArray.indexOf(asset) != assetsArray.size-1) {
              asset.concat("}")
            } else asset
          })
          assetsArray.foreach(asset => {
            val assetJson: Either[ParsingFailure, Json] = parse(asset)
            assetJson match {
              case Right(json) => {
                val id = (json \\ "id").head.toString()
                boxesMap.put(id, json)
              }
              case Left(e) => sys.error(s"Could not parse json: $e")
            }
          })
        }
        if (poly.nonEmpty) {
          val polyBox: Json = poly.head
          val id = (polyBox \\ "id").head.toString()
          boxesMap.put(id, polyBox)
        }
        if (arbit.nonEmpty) {
          val arbitBox: Json = arbit.head
          val id = (arbitBox \\ "id").head.toString()
          boxesMap.put(id, arbitBox)
        }
        walletBoxes(key) = boxesMap}
    })
    walletBoxes
  }

  override def receive: Receive = {

    case UpdateWallet(updatedBoxes) => sender ! parseUpdate(updatedBoxes)

    /*case UpdateWallet(add, remove) => {
      remove.foreach { case (publicKey, ids) =>
        walletBoxes.get(publicKey).map(boxes => ids.foreach(boxes.remove))
      }
      add.foreach { case (publicKey, newBoxes) =>
        walletBoxes.get(publicKey).map(boxes => newBoxes.foreach(box => boxes.put(box._1, box._2)))
      }
    }*/

  }
}

object WalletManager {

  case class UpdateWallet(updatedBoxes: Json)
  //case class UpdateWallet(add: MMap[String, MMap[String, Json]], remove: List[(String, List[String])])

}
