package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import co.topl.utils.HasName
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

case class CodeBox (override val evidence   : Evidence,
                    override val nonce      : Box.Nonce,
                    override val value      : ProgramId,
                    code                    : Seq[String], // List of strings of JS functions
                    interface               : Map[String, Seq[String]]
                   ) extends ProgramBox(evidence, nonce, value)

object CodeBox {
  val boxTypePrefix: BoxType = 13: Byte
  val boxTypeString: String = "CodeBox"

  implicit val name: HasName[CodeBox] = HasName.instance(() => boxTypeString)

  implicit val jsonEncoder: Encoder[CodeBox] = { box: CodeBox =>
    (Box.jsonEncode[ProgramId, CodeBox](box) ++ Map(
      "code" -> box.code.asJson,
      "interface" -> box.interface.map(ci => ci._1 -> ci._2.asJson).asJson
      )).asJson
  }

  implicit val jsonDecoder: Decoder[CodeBox] = ( c: HCursor ) =>
    for {
      b <- Box.jsonDecode[ProgramId](c)
      code <- c.downField("code").as[Seq[String]]
      interface <- c.downField("interface").as[Map[String, Seq[String]]]
    } yield {
      val (evidence, nonce, programId) = b
      CodeBox(evidence, nonce, programId, code, interface)
    }
}
