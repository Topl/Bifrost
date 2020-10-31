package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.serialization.BoxSerializer
import co.topl.utils.serialization.BifrostSerializer
import com.google.common.primitives.{ Ints, Longs }
import io.circe.syntax.EncoderOps
import io.circe.{ Decoder, DecodingFailure, Encoder, HCursor, Json }
import scorex.crypto.hash.Blake2b256

/**
 * Created by Matthew on 4/11/2017.
 */
abstract class Box[T] ( val evidence     : Evidence,
                        val value        : T,
                        val nonce        : Box.Nonce,
                        val boxTypePrefix: Box.BoxType
                      ) extends GenericBox[T] {

  type M = Box[_]

  lazy val id: BoxId = Box.idFromBox(this)

  lazy val json: Json = Box.jsonEncoder(this)

  def serializer: BifrostSerializer[Box[_]] = BoxSerializer

  override def hashCode(): Int = Ints.fromByteArray(bytes)
}


object Box {
  type Nonce = Long
  type BoxType = Byte

  def idFromBox[T] (box: Box[T]): BoxId = idFromPropNonce(box.evidence, box.nonce)

  def idFromPropNonce (evidence: Evidence, nonce: Box.Nonce): BoxId = {
    val hashBytes = Blake2b256(evidence.bytes ++ Longs.toByteArray(nonce))
    BoxId(hashBytes)
  }

  def jsonEncode(box: TokenBox): Map[String, Json] =
    Map(
      "id" -> box.id.toString.asJson,
      "type" -> prefixToTypeString(box.boxTypePrefix).asJson,
      "evidence" -> box.evidence.toString.asJson,
      "value" -> box.value.toString.asJson,
      "nonce" -> box.nonce.toString.asJson
      )

  def jsonDecode(c: HCursor): Either[DecodingFailure, (Evidence, Long, Long)] =
    for {
      evidence <- c.downField("evidence").as[Evidence]
      value <- c.downField("value").as[Long]
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

  implicit val jsonEncoder: Encoder[Box[_]] = {
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
