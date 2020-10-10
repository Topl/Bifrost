package co.topl.nodeView.state.box

import co.topl.crypto.{FastCryptographicHash, Signature25519}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import com.google.common.primitives.Longs
import io.circe.syntax.EncoderOps
import io.circe.{DecodingFailure, HCursor, Json}

 abstract class TokenBox(override val proposition: PublicKey25519Proposition,
                         override val nonce: Long,
                         override val value: Long
                        ) extends Box(proposition, nonce, value) {
  self =>

  lazy val id: BoxId = TokenBox.idFromBox(self)

}


object TokenBox {
  def idFromBox[PKP <: PublicKey25519Proposition] (box: TokenBox ): BoxId = idFromPropNonce(box.proposition, box.nonce)

  def idFromPropNonce (proposition: PublicKey25519Proposition, nonce: Long): BoxId = {
    val hashBytes = FastCryptographicHash(proposition.pubKeyBytes ++ Longs.toByteArray(nonce))
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

  def jsonDecode(c: HCursor): Either[DecodingFailure, (PublicKey25519Proposition, Long, Long)] =
    for {
      proposition <- c.downField("proposition").as[PublicKey25519Proposition]
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
  def generateUnlockers ( from: Seq[(PublicKey25519Proposition, Transaction.Nonce)],
                          signatures: Map[PublicKey25519Proposition, Signature25519]
                        ): Traversable[BoxUnlocker[PublicKey25519Proposition]] = {
    from.map {
      case (prop, nonce) =>
        val boxId = PublicKeyNoncedBox.idFromBox(prop, nonce)
        val boxKey = signatures.getOrElse(prop, throw new Exception("Signature not provided"))
        new BoxUnlocker(boxId, boxKey)
    }
  }

  def generateUnlockers ( boxIds   : Seq[BoxId],
                          signature: Signature25519
                        ): Traversable[BoxUnlocker[PublicKey25519Proposition]] = {
    boxIds.map { id =>
      new BoxUnlocker(id, signature)
    }
  }
}