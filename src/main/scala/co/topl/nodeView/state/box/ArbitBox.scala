package co.topl.nodeView.state.box

import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition

case class ArbitBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    override val value: Long) extends TokenBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "Arbit"
}
