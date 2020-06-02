package bifrost.modifier.box

import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.utils.serialization.BifrostSerializer

import scala.util.Try

case class PolyBox(override val proposition: PublicKey25519Proposition,
                   override val nonce: Long,
                   override val value: Long) extends NoncedBox(proposition, nonce, value) {
  override lazy val typeOfBox: String = "Poly"
}

object PolyBoxSerializer extends BifrostSerializer[PolyBox] with NoncedBoxSerializer {

  def toBytes(obj: PolyBox): Array[Byte] = {
    noncedBoxToBytes(obj, "PolyBox")
  }

  override def parseBytes(bytes: Array[Byte]): Try[PolyBox] = Try {
    val params = noncedBoxParseBytes(bytes)
    PolyBox(params._1, params._2, params._3)
  }

}
