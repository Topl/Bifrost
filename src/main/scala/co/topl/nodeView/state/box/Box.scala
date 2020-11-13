package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.{ BoxType, Nonce }
import co.topl.nodeView.state.box.serialization.BoxSerializer
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.Ints
import io.circe.syntax.EncoderOps
import io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json }

/**
 * Created by Matthew on 4/11/2017.
 */
sealed abstract class Box[T] ( val evidence     : Evidence,
                        val value        : T,
                        val nonce        : Nonce,
                        val boxTypePrefix: BoxType
                      ) extends GenericBox[T] {

  type M = Box[_]

  lazy val id: BoxId = BoxId(this)

  def serializer: BifrostSerializer[Box[_]] = BoxSerializer

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

abstract class TokenBox ( override val evidence     : Evidence,
                          override val nonce        : Nonce,
                          override val value        : TokenBox.Value,
                          override val boxTypePrefix: BoxType
                        ) extends Box[TokenBox.Value](evidence, nonce, value, boxTypePrefix)

abstract class ProgramBox (override val evidence     : Evidence,
                           override val nonce        : Nonce,
                           override val value        : ProgramId,
                           override val boxTypePrefix: BoxType
                          ) extends Box[ProgramId](evidence, value, nonce, boxTypePrefix)


object TokenBox {
  type Value = Long

  implicit def jsonEncoder: Encoder[TokenBox] = Box.jsonEncoder match {
    case enc: Encoder[TokenBox @unchecked] => enc
  }
}

object ProgramBox {
  implicit def jsonEncoder: Encoder[ProgramBox] = Box.jsonEncoder match {
    case enc: Encoder[ProgramBox @unchecked] => enc
  }
}


object Box {
  type Nonce = Long
  type BoxType = Byte

  def jsonEncode[T](box: Box[T]): Map[String, Json] =
    Map(
      "id" -> box.id.toString.asJson,
      "type" -> prefixToTypeString(box.boxTypePrefix).asJson,
      "evidence" -> box.evidence.toString.asJson,
      "value" -> box.value.toString.asJson,
      "nonce" -> box.nonce.toString.asJson
      )

  def jsonDecode[T](c: HCursor)(implicit valueDecoder: Decoder[T]): Either[DecodingFailure, (Evidence, Long, T)] =
    for {
      evidence <- c.downField("evidence").as[Evidence]
      value <- c.downField("value").as[T]
      nonce <- c.downField("issuer").as[Long]
    } yield {
      (evidence, nonce, value)
    }

  def prefixToTypeString(prefix: BoxType): String = prefix match {
    case ArbitBox.boxTypePrefix     => "ArbitBox"
    case PolyBox.boxTypePrefix      => "PolyBox"
    case AssetBox.boxTypePrefix     => "AssetBox"
    case ExecutionBox.boxTypePrefix => "ExecutionBox"
    case StateBox.boxTypePrefix     => "StateBox"
    case CodeBox.boxTypePrefix      => "CodeBox"
  }

  implicit def jsonEncoder: Encoder[Box[_]] = {
    case box: ArbitBox     => ArbitBox.jsonEncoder(box)
    case box: PolyBox      => PolyBox.jsonEncoder(box)
    case box: AssetBox     => AssetBox.jsonEncoder(box)
    case box: ExecutionBox => ExecutionBox.jsonEncoder(box)
    case box: StateBox     => StateBox.jsonEncoder(box)
    case box: CodeBox      => CodeBox.jsonEncoder(box)
  }

  implicit val jsonDecoder: Decoder[Box[_]] = { c: HCursor =>
    c.downField("typeOfBox").as[String].map {
      case "ArbitBox"     => ArbitBox.jsonDecoder(c)
      case "PolyBox"      => PolyBox.jsonDecoder(c)
      case "AssetBox"     => AssetBox.jsonDecoder(c)
      case "ExecutionBox" => ExecutionBox.jsonDecoder(c)
      case "StateBox"     => StateBox.jsonDecoder(c)
      case "CodeBox"      => CodeBox.jsonDecoder(c)
    } match {
      case Right(box) => box
      case Left(ex)   => throw ex
    }
  }
}
