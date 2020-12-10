package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.Nonce
import co.topl.nodeView.state.box.serialization.BoxSerializer
import co.topl.utils.HasName
import co.topl.utils.HasName.Syntax._
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.Ints
import io.circe._
import io.circe.syntax.EncoderOps

/** Created by Matthew on 4/11/2017.
  */
sealed abstract class Box[+T](val evidence: Evidence, val value: T, val nonce: Nonce)
    extends GenericBox[T] {

  type M = Box[_]

  lazy val id: BoxId = BoxId(this)

  def serializer: BifrostSerializer[Box[_]] = BoxSerializer

  override def toString: String =
    Box.boxName(this) + Box.jsonEncoder(this).noSpaces

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

  implicit val name: HasName[Box[_]] = (b: Box[_]) => {
    case bx: ArbitBox     => bx.name
    case bx: PolyBox      => bx.name
    case bx: AssetBox     => bx.name
  }

  implicit val jsonEncoder: Encoder[Box[_]] = {
    case box: ArbitBox     => ArbitBox.jsonEncoder(box)
    case box: PolyBox      => PolyBox.jsonEncoder(box)
    case box: AssetBox     => AssetBox.jsonEncoder(box)
    case _                 => throw new Exception("No matching encoder found")
  }

  implicit val jsonDecoder: Decoder[Box[_]] = { c: HCursor =>
    c.downField("type").as[String].map {
      case ArbitBox.boxTypeString     => ArbitBox.jsonDecoder(c)
      case PolyBox.boxTypeString      => PolyBox.jsonDecoder(c)
      case AssetBox.boxTypeString     => AssetBox.jsonDecoder(c)
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
  override val value:         T
) extends Box[T](evidence, value, nonce)

object TokenBox {
  implicit def jsonEncoder[T <: TokenValueHolder]: Encoder[TokenBox[T]] = (bx: TokenBox[_]) => Box.jsonEncoder(bx)
}

/* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

abstract class ProgramBox(
  override val evidence:      Evidence,
  override val nonce:         Nonce,
  override val value:         ProgramId
) extends Box[ProgramId](evidence, value, nonce)

object ProgramBox {
  implicit def jsonEncoder: Encoder[ProgramBox] = (bx: ProgramBox) => Box.jsonEncoder(bx)
}


