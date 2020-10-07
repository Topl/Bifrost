package co.topl.nodeView.state.box.proposition

import co.topl.utils.Extensions._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}
import scorex.crypto.signatures.PublicKey

object MofNPropositionSerializer extends BifrostSerializer[MofNProposition] {

  override def serialize(obj: MofNProposition, w: Writer): Unit = {
    /* m: Int */
    w.putUInt(obj.m)

    /* setOfPubKeyBytes: Set[Array[Byte]] */
    w.putUInt(obj.setOfPubKeyBytes.size)
    obj.setOfPubKeyBytes.foreach(b => w.putBytes(b))
  }

  override def parse(r: Reader): MofNProposition = {
    val m: Int = r.getUInt().toIntExact
    val n: Int = r.getUInt().toIntExact
    val setOfPubKeyBytes: Set[PublicKey] = (0 until n).map(_ => PublicKey @@ r.getBytes(Constants25519.PubKeyLength)).toSet

    MofNProposition(m, setOfPubKeyBytes)
  }
}
