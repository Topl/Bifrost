package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.Nonce
import co.topl.utils.Identifiable.Syntax._
import co.topl.utils.codecs.binary.legacy.BifrostSerializer
import co.topl.utils.codecs.binary.legacy.modifier.box.BoxSerializer
import co.topl.utils.{Identifiable, Identifier}
import com.google.common.primitives.Ints
import io.circe._
import io.circe.syntax.EncoderOps

/**
 * Created by Matthew on 4/11/2017.
 */
sealed abstract class Box[+T](val evidence: Evidence, val value: T, val nonce: Nonce) extends GenericBox[T] {

  type M = Box[_]

  lazy val id: BoxId = BoxId(this)

  def serializer: BifrostSerializer[Box[_]] = BoxSerializer

  override def toString: String =
    Box.identifier(this).typeString + Box.jsonEncoder(this).noSpaces

  override def hashCode(): Int = Ints.fromByteArray(bytes)

  override def equals(obj: Any): Boolean = obj match {
    case box: Box[_] => bytes sameElements box.bytes
    case _           => false
  }
}

object Box {
  type Nonce = Long
  type BoxType = Byte

  def jsonEncode[T: Encoder, BX <: Box[T]: Identifiable](box: BX): Map[String, Json] =
    Map(
      "id"       -> box.id.toString.asJson,
      "type"     -> box.getId.typeString.asJson,
      "evidence" -> box.evidence.toString.asJson,
      "value"    -> box.value.asJson,
      "nonce"    -> box.nonce.toString.asJson
    )

  def jsonDecode[T: Decoder](c: HCursor): Either[DecodingFailure, (Evidence, Nonce, T)] =
    for {
      evidence <- c.downField("evidence").as[Evidence]
      value    <- c.downField("value").as[T]
      nonce    <- c.downField("nonce").as[Nonce]
    } yield (evidence, nonce, value)

  def identifier(box: Box[_]): Identifier = box match {
    case _: ArbitBox     => ArbitBox.identifier.getId
    case _: PolyBox      => PolyBox.identifier.getId
    case _: AssetBox     => AssetBox.identifier.getId
    case _: ExecutionBox => ExecutionBox.identifier.getId
    case _: StateBox     => StateBox.identifier.getId
    case _: CodeBox      => CodeBox.identifier.getId
    case _               => throw new Exception("No matching identifier found")
  }

  implicit val jsonEncoder: Encoder[Box[_]] = {
    case box: ArbitBox     => ArbitBox.jsonEncoder(box)
    case box: PolyBox      => PolyBox.jsonEncoder(box)
    case box: AssetBox     => AssetBox.jsonEncoder(box)
    case box: ExecutionBox => ExecutionBox.jsonEncoder(box)
    case box: StateBox     => StateBox.jsonEncoder(box)
    case box: CodeBox      => CodeBox.jsonEncoder(box)
    case _                 => throw new Exception("No matching encoder found")
  }

  implicit val jsonDecoder: Decoder[Box[_]] = { c: HCursor =>
    c.downField("type").as[String].map {
      case ArbitBox.typeString     => ArbitBox.jsonDecoder(c)
      case PolyBox.typeString      => PolyBox.jsonDecoder(c)
      case AssetBox.typeString     => AssetBox.jsonDecoder(c)
      case ExecutionBox.typeString => ExecutionBox.jsonDecoder(c)
      case StateBox.typeString     => StateBox.jsonDecoder(c)
      case CodeBox.typeString      => CodeBox.jsonDecoder(c)
    } match {
      case Right(box) => box
      case Left(ex)   => throw ex
    }
  }
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

abstract class TokenBox[
  +T <: TokenValueHolder
](override val evidence: Evidence, override val nonce: Nonce, override val value: T)
    extends Box[T](evidence, value, nonce)

object TokenBox {
  implicit def jsonEncoder[T <: TokenValueHolder]: Encoder[TokenBox[T]] = (bx: TokenBox[_]) => Box.jsonEncoder(bx)
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

abstract class ProgramBox(
  override val evidence: Evidence,
  override val nonce:    Nonce,
  override val value:    ProgramId
) extends Box[ProgramId](evidence, value, nonce)

object ProgramBox {
  implicit def jsonEncoder: Encoder[ProgramBox] = (bx: ProgramBox) => Box.jsonEncoder(bx)
}
