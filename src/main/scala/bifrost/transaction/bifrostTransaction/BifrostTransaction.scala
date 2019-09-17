package bifrost.transaction.bifrostTransaction

import bifrost.BifrostApp
import bifrost.scorexMod.GenericBoxTransaction
import bifrost.settings.Settings
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.proof.Signature25519
import bifrost.transaction.state.PrivateKey25519
import com.google.common.primitives.Longs
import io.circe.Json
import io.circe.parser.parse
import scorex.crypto.encode.Base58

import scala.io.Source
import scala.util.Try

trait TransactionSettings extends Settings

trait BifrostTransaction
  extends GenericBoxTransaction[ProofOfKnowledgeProposition[PrivateKey25519], Any, BifrostBox] {
  lazy val bloomTopics: Option[IndexedSeq[Array[Byte]]] = None

  val boxIdsToOpen: IndexedSeq[Array[Byte]]

  implicit lazy val settings = new TransactionSettings {
    val testnetEndowment: Nonce = 20L
    override lazy val settingsJSON: Map[String, Json] = settingsFromFile(BifrostApp.settingsFilename)

    override def settingsFromFile(filename: String): Map[String, Json] = Try {
      val jsonString = Source.fromFile(filename).mkString
      parse(jsonString).right.get
    }
      .recoverWith {
        case _ =>
          Try {
            val jsonString = Source.fromURL(getClass.getResource(s"/$filename")).mkString
            parse(jsonString).right.get
          }
      }
      .toOption
      .flatMap(_.asObject)
      .map(_.toMap)
      .getOrElse(Map())
  }

}

object BifrostTransaction {
  type Nonce = Long
  type Value = Long

  def stringToPubKey(rawString: String): PublicKey25519Proposition =
    PublicKey25519Proposition(Base58.decode(rawString).get)

  def stringToSignature(rawString: String): Signature25519 = Signature25519(Base58.decode(rawString).get)

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(Longs.BYTES))
}