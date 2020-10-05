package co.topl.nodeView.state.box

import co.topl.nodeView.state.ProgramId
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor, Json }

case class CodeBox ( override val proposition: PublicKey25519Proposition,
                     override val nonce      : Long,
                     override val value      : ProgramId,
                     code                    : Seq[String], // List of strings of JS functions
                     interface               : Map[String, Seq[String]]
                   ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "CodeBox"
}

object CodeBox {

  implicit val jsonEncoder: Encoder[CodeBox] = (box: CodeBox) =>
    Map(
      "id" -> box.id.asJson,
      "type" -> box.typeOfBox.asJson,
      "proposition" -> box.proposition.asJson,
      "nonce" -> box.nonce.asJson,
      "programId" -> box.value.asJson,
      "code" -> box.code.asJson,
      "interface" -> box.interface.map(ci => ci._1 -> ci._2.asJson).asJson
      ).asJson

  implicit val jsonDecoder: Decoder[CodeBox] = ( c: HCursor ) =>
    for {
      proposition <- c.downField("proposition").as[PublicKey25519Proposition]
      value <- c.downField("programId").as[ProgramId]
      code <- c.downField("code").as[Seq[String]]
      interface <- c.downField("interface").as[Map[String, Seq[String]]]
      nonce <- c.downField("nonce").as[Long]
    } yield {
      CodeBox(proposition, nonce, value, code, interface)
    }
}
