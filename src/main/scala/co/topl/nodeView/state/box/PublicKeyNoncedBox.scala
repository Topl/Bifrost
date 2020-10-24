package co.topl.nodeView.state.box

import co.topl.crypto.proposition.PublicKey25519Proposition
import com.google.common.primitives.Longs
import scorex.crypto.hash.Blake2b256

trait PublicKeyNoncedBox[PKP <: PublicKey25519Proposition] extends GenericBox[PKP, Long] {
  val nonce: Long

  lazy val id: BoxId = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey = proposition

  override def equals(obj: Any): Boolean = obj match {
    case acc: PublicKeyNoncedBox[PKP] => (acc.id == this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}

object PublicKeyNoncedBox {

  def idFromBox[PKP <: PublicKey25519Proposition](prop: PKP, nonce: Long): BoxId = {
    val hashBytes = Blake2b256(prop.pubKeyBytes ++ Longs.toByteArray(nonce))
    BoxId(hashBytes)
  }
}