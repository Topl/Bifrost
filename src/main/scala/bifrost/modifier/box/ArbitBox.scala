package bifrost.modifier.box

import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

case class ArbitBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    override val value: Long) extends NoncedBox(proposition, nonce, value) {
  override lazy val typeOfBox: String = "Arbit"
}

object ArbitBoxSerializer extends BifrostSerializer[ArbitBox] {

  override def serialize(obj: ArbitBox, w: Writer): Unit = {
    w.putByteString("ArbitBox")
    NoncedBoxSerializer.serialize(obj, w)
  }

  override def parse(r: Reader): ArbitBox = {
    val noncedBox: NoncedBox = NoncedBoxSerializer.parse(r)
    ArbitBox(noncedBox.proposition, noncedBox.nonce, noncedBox.value)
  }

// TODO: Jing - remove
//
//  override def toBytes(obj: ArbitBox): Array[Byte] = {
//    noncedBoxToBytes(obj, "ArbitBox")
//  }
//
//  override def parseBytes(bytes: Array[Byte]): Try[ArbitBox] = Try {
//    val params = noncedBoxParseBytes(bytes)
//    ArbitBox(params._1, params._2, params._3)
//  }
}
