package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

case class CodeBox (override val evidence   : Evidence,
                    override val nonce      : Box.Nonce,
                    override val value      : ProgramId,
                    code                    : Seq[String], // List of strings of JS functions
                    interface               : Map[String, Seq[String]]
                   ) extends ProgramBox(evidence, value, nonce, CodeBox.boxTypePrefix)

object CodeBox {
  val boxTypePrefix: BoxType = 13: Byte

  implicit val jsonEncoder: Encoder[CodeBox] = { box: CodeBox =>
    (Box.jsonEncode(box) ++ Map(
      "code" -> box.code.asJson,
      "interface" -> box.interface.map(ci => ci._1 -> ci._2.asJson).asJson
      )).asJson
  }

  implicit val jsonDecoder: Decoder[CodeBox] = ( c: HCursor ) =>
    for {
      b <- Box.jsonDecode(c)
      code <- c.downField("code").as[Seq[String]]
      interface <- c.downField("interface").as[Map[String, Seq[String]]]
    } yield {
      val (proposition, nonce, programId) = b
      CodeBox(proposition, nonce, programId, code, interface)
    }
}
