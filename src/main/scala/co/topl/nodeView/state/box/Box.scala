package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.{BoxType, Nonce}
import co.topl.nodeView.state.box.serialization.BoxSerializer
import co.topl.utils.HasName
import co.topl.utils.HasName.Syntax._
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.Ints
import io.circe._
import io.circe.syntax.EncoderOps

/** Created by Matthew on 4/11/2017.
  */
sealed abstract class Box[+T](val evidence: Evidence, val value: T, val nonce: Nonce, val boxTypePrefix: BoxType)
    extends GenericBox[T] {

  type M = Box[_]

  lazy val id: BoxId = BoxId(this)

  def serializer: BifrostSerializer[Box[_]] = BoxSerializer

  override def toString: String =
    Box.prefixToTypeString(boxTypePrefix) +
    Box.jsonEncoder(this).noSpaces

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}

object Box {
  type Nonce = Long
  type BoxType = Byte

  def jsonEncode[T: Encoder, BX <: Box[T]: HasName](box: BX): Map[String, Json] =
    Map(
      "id"       -> box.id.toString.asJson,
      "type"     -> box.name.asJson,
      "evidence" -> box.evidence.toString.asJson,
      "value"    -> box.value.asJson,
      "nonce"    -> box.nonce.toString.asJson
    )

  def jsonDecode[T: Decoder](c: HCursor): Either[DecodingFailure, (Evidence, Nonce, T)] =
    for {
      evidence <- c.downField("evidence").as[Evidence]
      value    <- c.downField("value").as[T]
      nonce    <- c.downField("issuer").as[Nonce]
    } yield {
      (evidence, nonce, value)
    }

  def prefixToTypeString(prefix: BoxType): String = prefix match {
    case ArbitBox.boxTypePrefix     => ArbitBox.boxTypeString
    case PolyBox.boxTypePrefix      => PolyBox.boxTypeString
    case AssetBox.boxTypePrefix     => AssetBox.boxTypeString
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
    case _                 => throw new java.lang.Error(s"No matching encoder found")
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

/* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

abstract class TokenBox[
  +T <: TokenValueHolder
](override val evidence:      Evidence,
  override val nonce:         Nonce,
  override val value:         T,
  override val boxTypePrefix: BoxType
) extends Box[T](evidence, value, nonce, boxTypePrefix)

object TokenBox {
  implicit def jsonEncoder: Encoder[TokenBox[_ <: TokenValueHolder]] = (bx: TokenBox[_]) => Box.jsonEncoder(bx)
}

/* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

abstract class ProgramBox(
  override val evidence:      Evidence,
  override val nonce:         Nonce,
  override val value:         ProgramId,
  override val boxTypePrefix: BoxType
) extends Box[ProgramId](evidence, value, nonce, boxTypePrefix)

object ProgramBox {
  implicit def jsonEncoder: Encoder[ProgramBox] = (bx: ProgramBox) => Box.jsonEncoder(bx)
}


