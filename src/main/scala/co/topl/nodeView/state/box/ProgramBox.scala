package co.topl.nodeView.state.box

import co.topl.crypto.{FastCryptographicHash, Signature25519}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.ProgramId
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import com.google.common.primitives.Longs
import io.circe.syntax.EncoderOps
import io.circe.{DecodingFailure, HCursor, Json}

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

  def jsonEncode(box: ProgramBox): Map[String, Json] =
    Map(
      "id" -> box.id.toString.asJson,
      "type" -> box.typeOfBox.asJson,
      "proposition" -> box.proposition.toString.asJson,
      "nonce" -> box.nonce.asJson,
      "programId" -> box.value.toString.asJson,
    )

  def jsonDecode(c: HCursor): Either[DecodingFailure, (PublicKey25519Proposition, Long, ProgramId)] =
    for {
      proposition <- c.downField("proposition").as[PublicKey25519Proposition]
      nonce <- c.downField("nonce").as[Long]
      programId <- c.downField("programId").as[ProgramId]
    } yield {
      (proposition, nonce, programId)
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