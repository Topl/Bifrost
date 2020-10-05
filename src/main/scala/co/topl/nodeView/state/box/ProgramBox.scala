package co.topl.nodeView.state.box

import co.topl.crypto.FastCryptographicHash
import co.topl.nodeView.state.ProgramId
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import com.google.common.primitives.Longs

abstract class ProgramBox ( override val proposition: PublicKey25519Proposition,
                            override val nonce      : Long,
                            override val value      : ProgramId
                          ) extends Box(proposition, nonce, value) {
  self =>

  lazy val id: BoxId = ProgramBox.idFromBox(self)
}



object ProgramBox {
  def idFromBox[PKP <: PublicKey25519Proposition] (box: ProgramBox ): BoxId = {
    val hashBytes = FastCryptographicHash(
      box.proposition.pubKeyBytes ++
        box.typeOfBox.getBytes ++
        Longs.toByteArray(box.nonce))

    BoxId(hashBytes)
  }
}