package co.topl.nodeView.state.box

import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import io.circe.Json

/**
  * Created by cykoz on 5/15/2017.
  */

 abstract class TokenBox(override val proposition: PublicKey25519Proposition,
                         override val nonce: Long,
                         override val value: Long
                        ) extends Box(proposition, nonce, value) {

  lazy val id: BoxId = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  val json: Json

  val typeOfBox: String
}
