package bifrost.modifier.box

import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.utils.serialization.BifrostSerializer

import scala.util.Try

case class ArbitBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    override val value: Long) extends NoncedBox(proposition, nonce, value) {
  override lazy val typeOfBox: String = "Arbit"
}

object ArbitBoxSerializer extends BifrostSerializer[ArbitBox] with NoncedBoxSerializer {

  def toBytes(obj: ArbitBox): Array[Byte] = {
    noncedBoxToBytes(obj, "ArbitBox")
  }

  override def parseBytes(bytes: Array[Byte]): Try[ArbitBox] = Try {
    val params = noncedBoxParseBytes(bytes)
    ArbitBox(params._1, params._2, params._3)
  }

}
