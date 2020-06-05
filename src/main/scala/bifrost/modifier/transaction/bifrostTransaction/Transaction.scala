package bifrost.modifier.transaction.bifrostTransaction

import bifrost.crypto.{PrivateKey25519, PrivateKey25519Companion, Signature25519}
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.modifier.transaction.BoxTransaction
import bifrost.nodeView.NodeViewModifier
import bifrost.nodeView.NodeViewModifier.ModifierTypeId
import bifrost.settings.Settings
import bifrost.wallet.Wallet
import com.google.common.primitives.Longs
import scorex.crypto.encode.Base58

trait TransactionSettings extends Settings

trait Transaction
  extends BoxTransaction[ProofOfKnowledgeProposition[PrivateKey25519], Any, Box] {
  lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = None

  lazy override val modifierTypeId: ModifierTypeId = Transaction.modifierTypeId

  val boxIdsToOpen: IndexedSeq[Array[Byte]]
}

object Transaction {
  type Nonce = Long
  type Value = Long

  val modifierTypeId = ModifierTypeId @@ (2: Byte)

  def stringToPubKey(rawString: String): PublicKey25519Proposition =
    PublicKey25519Proposition(Base58.decode(rawString).get)

  def stringToSignature(rawString: String): Signature25519 = Signature25519(Base58.decode(rawString).get)

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))

  def signTx(w: Wallet, props: IndexedSeq[PublicKey25519Proposition], message: Array[Byte]):
  Map[PublicKey25519Proposition, Signature25519] = props.map { prop =>
    val secret = w.secretByPublicImage(prop).get
    val signature = PrivateKey25519Companion.sign(secret, message)
    prop -> signature
  }.toMap
}
