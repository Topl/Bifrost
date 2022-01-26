package co.topl.codecs.json.modifier.box

import co.topl.attestation.Evidence
import co.topl.codecs.binary._
import co.topl.codecs.json.valuetypes._
import co.topl.codecs.json.{
  deriveDecoderFromScodec,
  deriveEncoderFromScodec,
  deriveKeyDecoderFromScodec,
  deriveKeyEncoderFromScodec
}
import co.topl.modifier.box.Box.Nonce
import co.topl.modifier.box._
import co.topl.modifier.transaction.builder.{BoxSelectionAlgorithm, BoxSelectionAlgorithms}
import co.topl.utils.Identifiable.Syntax._
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import co.topl.utils.{Identifiable, Int128}
import io.circe._
import io.circe.syntax._

trait BoxCodecs {

  private val assetCodeTypeName = "Asset Code"
  private val boxIdTypeName = "Box ID"
  private val securityRootTypeName = "Security Root"

  implicit val arbitBoxJsonEncoder: Encoder[ArbitBox] = (box: ArbitBox) =>
    jsonEncodeBox[SimpleValue, ArbitBox](box).asJson

  implicit val arbitBoxJsonDecoder: Decoder[ArbitBox] = (c: HCursor) =>
    jsonDecodeBox[SimpleValue](c).map { case (evidence, nonce, value) =>
      ArbitBox(evidence, nonce, value)
    }

  implicit val assetBoxJsonEncoder: Encoder[AssetBox] = (box: AssetBox) =>
    jsonEncodeBox[AssetValue, AssetBox](box).asJson

  implicit val assetBoxJsonDecoder: Decoder[AssetBox] = (c: HCursor) =>
    jsonDecodeBox[AssetValue](c).map { case (evidence, nonce, value) =>
      AssetBox(evidence, nonce, value)
    }

  implicit val assetCodeJsonEncoder: Encoder[AssetCode] = (ac: AssetCode) => ac.toString.asJson

  implicit val assetCodeJsonKeyEncoder: KeyEncoder[AssetCode] = (ac: AssetCode) => ac.toString

  implicit val assetCodeJsonDecoder: Decoder[AssetCode] = deriveDecoderFromScodec(assetCodeTypeName)

  implicit val assetCodeJsonKeyDecoder: KeyDecoder[AssetCode] = deriveKeyDecoderFromScodec(assetCodeTypeName)

  implicit val codeBoxJsonEncoder: Encoder[CodeBox] = { box: CodeBox =>
    (jsonEncodeBox[ProgramId, CodeBox](box) ++ Map(
      "code"      -> box.code.asJson,
      "interface" -> box.interface.map(ci => ci._1 -> ci._2.asJson).asJson
    )).asJson
  }

  implicit val codeBoxJsonDecoder: Decoder[CodeBox] = (c: HCursor) =>
    for {
      b         <- jsonDecodeBox[ProgramId](c)
      code      <- c.downField("code").as[Seq[String]]
      interface <- c.downField("interface").as[Map[String, Seq[String]]]
    } yield {
      val (evidence, nonce, programId) = b
      CodeBox(evidence, nonce, programId, code, interface)
    }

  implicit val executionBoxJsonEncoder: Encoder[ExecutionBox] = { box: ExecutionBox =>
    (jsonEncodeBox[ProgramId, ExecutionBox](box) ++ Map(
      "stateBoxIds" -> box.stateBoxIds.asJson,
      "codeBoxIds"  -> box.codeBoxIds.asJson
    )).asJson
  }

  implicit val executionBoxJsonDecoder: Decoder[ExecutionBox] = (c: HCursor) =>
    for {
      b           <- jsonDecodeBox[ProgramId](c)
      stateBoxIds <- c.downField("stateBoxIds").as[Seq[ProgramId]]
      codeBoxIds  <- c.downField("codeBoxIds").as[Seq[ProgramId]]
    } yield {
      val (evidence, nonce, programId) = b
      ExecutionBox(evidence, nonce, programId, stateBoxIds, codeBoxIds)
    }

  implicit val polyBoxJsonEncoder: Encoder[PolyBox] = (box: PolyBox) => jsonEncodeBox[SimpleValue, PolyBox](box).asJson

  implicit val polyBoxJsonDecoder: Decoder[PolyBox] = (c: HCursor) =>
    jsonDecodeBox[SimpleValue](c).map { case (evidence, nonce, value) =>
      PolyBox(evidence, nonce, value)
    }

  implicit val programIdJsonEncoder: Encoder[ProgramId] = (id: ProgramId) => id.toString.asJson

  implicit val programIdJsonKeyEncoder: KeyEncoder[ProgramId] = (id: ProgramId) => id.toString

  implicit val programIdJsonDecoder: Decoder[ProgramId] =
    Decoder.decodeString
      .emap(Base58Data.validated(_).leftMap(_ => "Value is not Base 58").toEither)
      .map(ProgramId.apply)

  implicit val programIdJsonKeyDecoder: KeyDecoder[ProgramId] = (id: String) =>
    Base58Data.validated(id).map(ProgramId.apply).toOption

  implicit val stateBoxJsonEncoder: Encoder[StateBox] = { box: StateBox =>
    (jsonEncodeBox[ProgramId, StateBox](box) ++ Map(
      "state" -> box.state.asJson
    )).asJson
  }

  implicit val stateBoxJsonDecoder: Decoder[StateBox] = (c: HCursor) =>
    for {
      b     <- jsonDecodeBox[ProgramId](c)
      state <- c.downField("state").as[Json]
    } yield {
      val (evidence, nonce, programId) = b
      StateBox(evidence, nonce, programId, state)
    }

  implicit val tokenValueHolderJsonEncoder: Encoder[TokenValueHolder] = {
    case v: SimpleValue => simpleValueJsonEncoder(v)
    case v: AssetValue  => assetValueJsonEncoder(v)
    case _              => throw new Exception(s"No matching encoder found")
  }

  implicit val tokenValueHolderJsonDecoder: Decoder[TokenValueHolder] = { c: HCursor =>
    c.downField("type").as[String].map {
      case SimpleValue.valueTypeString => simpleValueJsonDecoder(c)
      case AssetValue.valueTypeString  => assetValueJsonDecoder(c)
    } match {
      case Right(v) => v
      case Left(ex) => throw ex
    }
  }

  implicit val simpleValueJsonEncoder: Encoder[SimpleValue] = { (value: SimpleValue) =>
    Map(
      "type"     -> SimpleValue.valueTypeString.asJson,
      "quantity" -> value.quantity.asJson
    ).asJson
  }

  implicit val simpleValueJsonDecoder: Decoder[SimpleValue] = (c: HCursor) =>
    for {
      quantity <- c.downField("quantity").as[Long]
    } yield SimpleValue(quantity)

  implicit val assetValueJsonEncoder: Encoder[AssetValue] = { (value: AssetValue) =>
    Map(
      "type"         -> AssetValue.valueTypeString.asJson,
      "quantity"     -> value.quantity.asJson,
      "assetCode"    -> value.assetCode.asJson,
      "securityRoot" -> value.securityRoot.asJson,
      "metadata"     -> value.metadata.asJson
    ).asJson
  }

  implicit val assetValueJsonDecoder: Decoder[AssetValue] = (c: HCursor) =>
    for {
      quantity     <- c.get[Int128]("quantity")
      assetCode    <- c.downField("assetCode").as[AssetCode]
      securityRoot <- c.downField("securityRoot").as[Option[SecurityRoot]]
      metadata     <- c.downField("metadata").as[Option[Latin1Data]]
    } yield AssetValue(quantity, assetCode, securityRoot.getOrElse(SecurityRoot.empty), metadata)

  implicit val boxJsonEncoder: Encoder[Box[_]] = {
    case box: ArbitBox     => arbitBoxJsonEncoder(box)
    case box: PolyBox      => polyBoxJsonEncoder(box)
    case box: AssetBox     => assetBoxJsonEncoder(box)
    case box: ExecutionBox => executionBoxJsonEncoder(box)
    case box: StateBox     => stateBoxJsonEncoder(box)
    case box: CodeBox      => codeBoxJsonEncoder(box)
    case _                 => throw new Exception("No matching encoder found")
  }

  implicit val boxJsonDecoder: Decoder[Box[_]] = { c: HCursor =>
    c.downField("type").as[String].map {
      case ArbitBox.typeString     => arbitBoxJsonDecoder(c)
      case PolyBox.typeString      => polyBoxJsonDecoder(c)
      case AssetBox.typeString     => assetBoxJsonDecoder(c)
      case ExecutionBox.typeString => executionBoxJsonDecoder(c)
      case StateBox.typeString     => stateBoxJsonDecoder(c)
      case CodeBox.typeString      => codeBoxJsonDecoder(c)
    } match {
      case Right(box) => box
      case Left(ex)   => throw ex
    }
  }

  implicit def tokenBoxJsonEncoder[T <: TokenValueHolder]: Encoder[TokenBox[T]] = {
    case assetBox: AssetBox => assetBoxJsonEncoder(assetBox)
    case arbitBox: ArbitBox => arbitBoxJsonEncoder(arbitBox)
    case polyBox: PolyBox   => polyBoxJsonEncoder(polyBox)
    case _                  => throw new Exception("no encoder found for token box type")
  }

  implicit val programBoxJsonEncoder: Encoder[ProgramBox] = {
    case stateBox: StateBox         => stateBoxJsonEncoder(stateBox)
    case executionBox: ExecutionBox => executionBoxJsonEncoder(executionBox)
    case codeBox: CodeBox           => codeBoxJsonEncoder(codeBox)
    case _                          => throw new Exception("no encoder found for program box type")
  }

  implicit val boxIdJsonEncoder: Encoder[BoxId] = deriveEncoderFromScodec(boxIdTypeName)

  implicit val boxIdJsonKeyEncoder: KeyEncoder[BoxId] = deriveKeyEncoderFromScodec(boxIdTypeName)

  implicit val boxIdJsonDecoder: Decoder[BoxId] = deriveDecoderFromScodec(boxIdTypeName)

  implicit val boxIdJsonKeyDecoder: KeyDecoder[BoxId] = deriveKeyDecoderFromScodec(boxIdTypeName)

  implicit val boxSelectionAlgorithmJsonEncoder: Encoder[BoxSelectionAlgorithm] = {
    case BoxSelectionAlgorithms.All           => "All".asJson
    case BoxSelectionAlgorithms.LargestFirst  => "LargestFirst".asJson
    case BoxSelectionAlgorithms.SmallestFirst => "SmallestFirst".asJson
    case BoxSelectionAlgorithms.Specific(ids) =>
      Json.obj(
        "Specific" -> Json.obj(
          "ids" -> ids.asJson
        )
      )
  }

  implicit val boxSelectionAlgorithmJsonDecoder: Decoder[BoxSelectionAlgorithm] =
    List[Decoder[BoxSelectionAlgorithm]](
      Decoder[String].emap(str => Either.cond(str == "All", BoxSelectionAlgorithms.All, "value is not of type 'All'")),
      Decoder[String].emap(str =>
        Either.cond(str == "LargestFirst", BoxSelectionAlgorithms.LargestFirst, "value is not of type 'LargestFirst'")
      ),
      Decoder[String].emap(str =>
        Either.cond(
          str == "SmallestFirst",
          BoxSelectionAlgorithms.SmallestFirst,
          "value is not of type 'SmallestFirst'"
        )
      ),
      Decoder.instance(hcursor =>
        hcursor
          .downField("Specific")
          .downField("ids")
          .as[List[BoxId]]
          .map(BoxSelectionAlgorithms.Specific)
      )
    ).reduceLeft(_ or _)

  implicit val securityRootJsonEncoder: Encoder[SecurityRoot] = deriveEncoderFromScodec(securityRootTypeName)

  implicit val securityRootJsonDecoder: Decoder[SecurityRoot] = deriveDecoderFromScodec(securityRootTypeName)

  private def jsonEncodeBox[T: Encoder, BX <: Box[T]: Identifiable](box: BX): Map[String, Json] =
    Map(
      "id"       -> box.id.asJson,
      "type"     -> box.getId.typeString.asJson,
      "evidence" -> box.evidence.asJson,
      "value"    -> box.value.asJson,
      "nonce"    -> box.nonce.asJson
    )

  private def jsonDecodeBox[T: Decoder](c: HCursor): Either[DecodingFailure, (Evidence, Nonce, T)] =
    for {
      evidence <- c.downField("evidence").as[Evidence]
      value    <- c.downField("value").as[T]
      nonce    <- c.downField("nonce").as[Nonce]
    } yield (evidence, nonce, value)
}
