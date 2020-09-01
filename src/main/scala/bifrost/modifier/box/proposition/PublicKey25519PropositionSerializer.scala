package bifrost.modifier.box.proposition

import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object PublicKey25519PropositionSerializer extends BifrostSerializer[PublicKey25519Proposition] {

  override def serialize(obj: PublicKey25519Proposition, w: Writer): Unit = {
    w.putBytes(obj.pubKeyBytes)
  }

  override def parse(r: Reader): PublicKey25519Proposition = {
    PublicKey25519Proposition(r.getBytes(Constants25519.PubKeyLength))
  }

// TODO: Jing - remove
//
//  override def toBytes(obj: PublicKey25519Proposition): Array[Byte] = obj.pubKeyBytes
//
//  override def parseBytes(bytes: Array[Byte]): Try[PublicKey25519Proposition] = Try(PublicKey25519Proposition(bytes))
}
