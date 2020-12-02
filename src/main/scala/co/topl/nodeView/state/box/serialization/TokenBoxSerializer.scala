package co.topl.nodeView.state.box.serialization

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.TokenBox
import co.topl.utils.serialization.{Reader, Writer}

object TokenBoxSerializer {

  def serialize(obj: TokenBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    Evidence.serialize(obj.evidence, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    w.putULong(obj.value)
  }

  def parse(r: Reader): (Evidence, Long, Long) = {
    (Evidence.parse(r), r.getLong(), r.getULong())
  }
}
