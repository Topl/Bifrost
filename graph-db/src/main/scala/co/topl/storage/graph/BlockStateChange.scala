package co.topl.storage.graph

import cats.kernel.Semigroup
import co.topl.crypto.hash.blake2b256

import java.nio.charset.StandardCharsets

case class BlockStateChange(boxesOpened: Set[String], boxesCreated: Set[String]) {

  def hash: Array[Byte] = {
    val messages = (boxesOpened.toList.sorted ++ boxesCreated.toList.sorted).map(_.getBytes(StandardCharsets.UTF_8))
    blake2b256.hash(None, messages: _*).value
  }
}

object BlockStateChange {

  implicit val semigroup: Semigroup[BlockStateChange] =
    (x, y) => BlockStateChange(x.boxesOpened ++ y.boxesOpened, x.boxesCreated ++ y.boxesCreated)

}
