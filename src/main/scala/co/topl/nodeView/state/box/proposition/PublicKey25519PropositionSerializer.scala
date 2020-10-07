package co.topl.nodeView.state.box.proposition

import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }
import scorex.crypto.signatures.PublicKey

object PublicKey25519PropositionSerializer extends BifrostSerializer[PublicKey25519Proposition] {

  override def serialize(obj: PublicKey25519Proposition, w: Writer): Unit = {
    w.putBytes(obj.pubKeyBytes)
  }

  override def parse(r: Reader): PublicKey25519Proposition = {
    PublicKey25519Proposition(PublicKey @@ r.getBytes(Constants25519.PubKeyLength))
  }
}
