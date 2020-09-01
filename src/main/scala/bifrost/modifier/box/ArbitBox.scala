package bifrost.modifier.box

import bifrost.modifier.box.proposition.PublicKey25519Proposition

case class ArbitBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    override val value: Long) extends NoncedBox(proposition, nonce, value) {
  override lazy val typeOfBox: String = "Arbit"
}
