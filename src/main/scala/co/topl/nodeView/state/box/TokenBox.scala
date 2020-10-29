package co.topl.nodeView.state.box

import co.topl.attestation.{BoxUnlocker, KnowledgeProposition, Secret}
import co.topl.attestation.ToplAddress.AddressContent
import co.topl.attestation.address.PublicKeyAddress
import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
import co.topl.attestation.proof.SignatureCurve25519
import co.topl.attestation.secrets.PrivateKeyCurve25519
import co.topl.modifier.transaction.Transaction
import com.google.common.primitives.Longs
import io.circe.syntax.EncoderOps
import io.circe.{DecodingFailure, HCursor, Json}
import scorex.crypto.hash.Blake2b256

 abstract class TokenBox( override val proposition: KnowledgeProposition[_ <: Secret],
                          override val nonce: Box.Nonce,
                          override val value: TokenBox.Value
                        ) extends Box(proposition, nonce, value) {

  lazy val id: BoxId = TokenBox.idFromBox(this)

}


object TokenBox {
  type Value = Long

  def idFromBox[PKP <: KnowledgeProposition[PrivateKeyCurve25519]] (box: TokenBox ): BoxId = idFromPropNonce(box.proposition, box.nonce)

  def idFromPropNonce (proposition: KnowledgeProposition[PrivateKeyCurve25519], nonce: Long): BoxId = {
    val hashBytes = Blake2b256(proposition.pubKeyBytes ++ Longs.toByteArray(nonce))
    BoxId(hashBytes)
  }

  def jsonEncode(box: TokenBox): Map[String, Json] =
    Map(
      "id" -> box.id.toString.asJson,
      "type" -> box.typeOfBox.asJson,
      "proposition" -> box.proposition.toString.asJson,
      "value" -> box.value.toString.asJson,
      "nonce" -> box.nonce.toString.asJson
    )

  def jsonDecode(c: HCursor): Either[DecodingFailure, (PublicKeyCurve25519Proposition, Long, Long)] =
    for {
      proposition <- c.downField("proposition").as[PublicKeyCurve25519Proposition]
      value <- c.downField("value").as[Long]
      nonce <- c.downField("issuer").as[Long]
    } yield {
      (proposition, nonce, value)
    }

  /**
   * Generate a series of unlockers for a transactions that is used to validate the transaction
   *
   * @param from
   * @param signatures
   * @return
   */
  def generateUnlockers (from: Seq[(PublicKeyCurve25519Proposition, Transaction.Nonce)],
                         signatures: Map[PublicKeyCurve25519Proposition, SignatureCurve25519]
                        ): Traversable[BoxUnlocker[PublicKeyCurve25519Proposition]] = {
    from.map {
      case (prop, nonce) =>
        val boxId = PublicKeyNoncedBox.idFromBox(prop, nonce)
        val boxKey = signatures.getOrElse(prop, throw new Exception("Signature not provided"))
        new BoxUnlocker(boxId, boxKey)
    }
  }

  def generateUnlockers ( boxIds   : Seq[BoxId],
                          signature: SignatureCurve25519
                        ): Traversable[BoxUnlocker[PublicKeyCurve25519Proposition]] = {
    boxIds.map { id =>
      new BoxUnlocker(id, signature)
    }
  }
}