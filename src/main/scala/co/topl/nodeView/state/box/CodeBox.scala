package co.topl.nodeView.state.box

import co.topl.attestation.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.ProgramId
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor }

case class CodeBox ( override val proposition: PublicKey25519Proposition,
                     override val nonce      : Long,
                     override val value      : ProgramId,
                     code                    : Seq[String], // List of strings of JS functions
                     interface               : Map[String, Seq[String]]
                   ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "CodeBox"
}

object CodeBox {

  implicit val jsonEncoder: Encoder[CodeBox] = { box: CodeBox =>
    (ProgramBox.jsonEncode(box) ++ Map(
      "code" -> box.code.asJson,
      "interface" -> box.interface.map(ci => ci._1 -> ci._2.asJson).asJson
      )).asJson
  }

  implicit val jsonDecoder: Decoder[CodeBox] = ( c: HCursor ) =>
    for {
      b <- ProgramBox.jsonDecode(c)
      code <- c.downField("code").as[Seq[String]]
      interface <- c.downField("interface").as[Map[String, Seq[String]]]
    } yield {
      val (proposition, nonce, programId) = b
      CodeBox(proposition, nonce, programId, code, interface)
    }
}
