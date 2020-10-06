package co.topl.nodeView.state.box

import co.topl.crypto.FastCryptographicHash
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import com.google.common.primitives.Longs

/**
  * Created by cykoz on 5/15/2017.
  */

 abstract class TokenBox(override val proposition: PublicKey25519Proposition,
                         override val nonce: Long,
                         override val value: Long
                        ) extends Box(proposition, nonce, value) {
  self =>

  lazy val id: BoxId = TokenBox.idFromBox(self)

}


object TokenBox {
  def idFromBox[PKP <: PublicKey25519Proposition] (box: TokenBox ): BoxId = {
    val hashBytes = FastCryptographicHash(
      box.proposition.pubKeyBytes ++
        box.typeOfBox.getBytes ++
        Longs.toByteArray(box.nonce))

    BoxId(hashBytes)
  }
}