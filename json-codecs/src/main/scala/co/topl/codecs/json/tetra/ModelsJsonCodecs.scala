package co.topl.codecs.json.tetra

import cats.data.Chain
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{HasLength, Length, Lengths, Sized}
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

import scala.collection.immutable.ListSet

trait ModelsJsonCodecs {

  implicit val bytesEncoder: Encoder[Bytes] =
    _.toBase58.asJson

  implicit val bytesDecoder: Decoder[Bytes] =
    h => h.as[String].flatMap(Bytes.fromBase58(_).toRight(DecodingFailure("Not base58", Nil)))

  implicit def sizedStrictDecoder[T: HasLength: Decoder, L <: Length](implicit l: L): Decoder[Sized.Strict[T, L]] =
    _.as[T].flatMap(t => Sized.strict[T, L](t).leftMap(f => DecodingFailure(f.toString, Nil)))

  implicit def sizedStrictEncoder[T: Encoder, L <: Length]: Encoder[Sized.Strict[T, L]] =
    _.data.asJson

  implicit def sizedMaxDecoder[T: HasLength: Decoder, L <: Length](implicit l: L): Decoder[Sized.Max[T, L]] =
    _.as[T].flatMap(t => Sized.max[T, L](t).leftMap(f => DecodingFailure(f.toString, Nil)))

  implicit def sizedMaxEncoder[T: Encoder, L <: Length]: Encoder[Sized.Max[T, L]] =
    _.data.asJson

  implicit val typedBytesEncoder: Encoder[TypedBytes] =
    bytes => bytes.allBytes.asJson

  implicit val typedBytesDecoder: Decoder[TypedBytes] =
    h => h.as[Bytes].map(TypedBytes(_))

  implicit val typedEvidenceEncoder: Encoder[TypedEvidence] =
    bytes => bytes.allBytes.asJson

  implicit val spendingAddressEncoder: Encoder[SpendingAddress] =
    t => t.immutableBytes.asJson

  implicit val spendingAddressDecoder: Decoder[SpendingAddress] =
    t => t.as[Bytes].flatMap(_.decodeImmutable[SpendingAddress].leftMap(e => DecodingFailure(e, Nil)))

  implicit val fullAddressEncoder: Encoder[FullAddress] =
    t => t.immutableBytes.asJson

  implicit val fullAddressDecoder: Decoder[FullAddress] =
    t => t.as[Bytes].flatMap(_.decodeImmutable[FullAddress].leftMap(e => DecodingFailure(e, Nil)))

  implicit val verificationKeysCurve25519Encoder: Encoder[VerificationKeys.Curve25519] =
    t => t.bytes.data.toBase58.asJson

  implicit val verificationKeysCurve25519Decoder: Decoder[VerificationKeys.Curve25519] =
    Decoder[Sized.Strict[Bytes, VerificationKeys.Curve25519.Length]].map(VerificationKeys.Curve25519.apply)

  implicit val verificationKeysEd25519Encoder: Encoder[VerificationKeys.Ed25519] =
    t => t.bytes.data.toBase58.asJson

  implicit val verificationKeysEd25519Decoder: Decoder[VerificationKeys.Ed25519] =
    Decoder[Sized.Strict[Bytes, VerificationKeys.Ed25519.Length]].map(VerificationKeys.Ed25519.apply)

  implicit val verificationKeysExtendedEd25519Encoder: Encoder[VerificationKeys.ExtendedEd25519] =
    t =>
      Json.obj(
        "vk"        -> t.vk.bytes.data.toBase58.asJson,
        "chainCode" -> t.chainCode.data.toBase58.asJson
      )

  implicit val verificationKeysExtendedEd25519Decoder: Decoder[VerificationKeys.ExtendedEd25519] =
    hcursor =>
      for {
        vk <- hcursor.downField("vk").as[VerificationKeys.Ed25519]
        chainCode <- hcursor
          .downField("chainCode")
          .as[Sized.Strict[Bytes, VerificationKeys.ExtendedEd25519.ChainCodeLength]]
      } yield VerificationKeys.ExtendedEd25519(vk, chainCode)

  implicit val propositionsKnowledgeCurve25519Encoder: Encoder[Propositions.Knowledge.Curve25519] =
    deriveEncoder

  implicit val propositionsKnowledgeCurve25519Decoder: Decoder[Propositions.Knowledge.Curve25519] =
    deriveDecoder

  implicit val propositionsKnowledgeEd25519Encoder: Encoder[Propositions.Knowledge.Ed25519] =
    deriveEncoder

  implicit val propositionsKnowledgeEd25519Decoder: Decoder[Propositions.Knowledge.Ed25519] =
    deriveDecoder

  implicit val propositionsKnowledgeExtendedEd25519Encoder: Encoder[Propositions.Knowledge.ExtendedEd25519] =
    deriveEncoder

  implicit val propositionsKnowledgeExtendedEd25519Decoder: Decoder[Propositions.Knowledge.ExtendedEd25519] =
    deriveDecoder

  implicit def propositionEncoder: Encoder[Proposition] = {
    case Propositions.PermanentlyLocked =>
      Json.obj("propositionType" -> "PermanentlyLocked".asJson)
    case Propositions.Knowledge.Curve25519(k) =>
      Json.obj("propositionType" -> "Knowledge.Curve25519".asJson, "key" -> k.bytes.data.asJson)
    case Propositions.Knowledge.Ed25519(k) =>
      Json.obj("propositionType" -> "Knowledge.Ed25519".asJson, "key" -> k.bytes.data.asJson)
    case Propositions.Knowledge.ExtendedEd25519(k) =>
      Json.obj(
        "propositionType" -> "Knowledge.ExtendedEd25519".asJson,
        "key" -> Json.obj(
          "vk"        -> k.vk.bytes.data.asJson,
          "chainCode" -> k.chainCode.data.asJson
        )
      )
    case Propositions.Knowledge.HashLock(digest) =>
      Json.obj(
        "propositionType" -> "Knowledge.HashLock".asJson,
        "digest"          -> digest.data.asJson
      )
    case Propositions.Compositional.Threshold(threshold, propositions) =>
      Json.obj(
        "propositionType" -> "Compositional.Threshold".asJson,
        "threshold"       -> threshold.asJson,
        "propositions"    -> propositions.toList.asJson(Encoder.encodeList(propositionEncoder))
      )
    case Propositions.Compositional.And(a, b) =>
      Json.obj(
        "propositionType" -> "Compositional.And".asJson,
        "a"               -> a.asJson(propositionEncoder),
        "b"               -> b.asJson(propositionEncoder)
      )
    case Propositions.Compositional.Or(a, b) =>
      Json.obj(
        "propositionType" -> "Compositional.Or".asJson,
        "a"               -> a.asJson(propositionEncoder),
        "b"               -> b.asJson(propositionEncoder)
      )
    case Propositions.Compositional.Not(a) =>
      Json.obj(
        "propositionType" -> "Compositional.Not".asJson,
        "a"               -> a.asJson(propositionEncoder)
      )
    case Propositions.Contextual.HeightLock(height) =>
      Json.obj(
        "propositionType" -> "Contextual.HeightLock".asJson,
        "height"          -> height.asJson
      )
    case Propositions.Contextual.RequiredTransactionIO(boxes) =>
      Json.obj(
        "propositionType" -> "Contextual.RequiredTransactionIO".asJson,
        "boxes" -> boxes.map { case (b, location) =>
          Json.obj(
            "box" -> b.asJson,
            "location" -> (location match {
              case _: BoxLocations.Input  => "input"
              case _: BoxLocations.Output => "output"
            }).asJson,
            "index" -> (location match {
              case BoxLocations.Input(index)  => index
              case BoxLocations.Output(index) => index
            }).asJson
          )
        }.asJson
      )
  }

  implicit val propositionDecoder: Decoder[Proposition] =
    hcursor =>
      hcursor.downField("propositionType").as[String].flatMap {
        case "PermanentlyLocked"         => Propositions.PermanentlyLocked.asRight
        case "Knowledge.Curve25519"      => hcursor.downField("key").as[Propositions.Knowledge.Curve25519]
        case "Knowledge.Ed25519"         => hcursor.downField("key").as[Propositions.Knowledge.Ed25519]
        case "Knowledge.ExtendedEd25519" => hcursor.downField("key").as[Propositions.Knowledge.ExtendedEd25519]
        case "Compositional.Threshold" =>
          (
            hcursor.downField("threshold").as[Int],
            hcursor
              .downField("propositions")
              .as[ListSet[Proposition]](Decoder.decodeList(propositionDecoder).map(ListSet.empty[Proposition] ++ _))
          ).mapN((threshold, propositions) => Propositions.Compositional.Threshold(threshold, propositions))
        case "Compositional.And" =>
          (
            hcursor.downField("a").as[Proposition](propositionDecoder),
            hcursor.downField("b").as[Proposition](propositionDecoder)
          ).mapN((a, b) => Propositions.Compositional.And(a, b))
        case "Compositional.Or" =>
          (
            hcursor.downField("a").as[Proposition](propositionDecoder),
            hcursor.downField("b").as[Proposition](propositionDecoder)
          ).mapN((a, b) => Propositions.Compositional.Or(a, b))
        case "Contextual.HeightLock" =>
          hcursor.downField("height").as[Long].map(Propositions.Contextual.HeightLock.apply)
      }

  implicit def proofEncoder: Encoder[Proof] = {
    case Proofs.Undefined =>
      Json.obj(
        "proofType" -> "False".asJson
      )
    case Proofs.Knowledge.Curve25519(bytes) =>
      Json.obj(
        "proofType" -> "Knowledge.Curve25519".asJson,
        "signature" -> bytes.data.asJson
      )
    case Proofs.Knowledge.Ed25519(bytes) =>
      Json.obj(
        "proofType" -> "Knowledge.Ed25519".asJson,
        "signature" -> bytes.data.asJson
      )
    case Proofs.Knowledge.VrfEd25519(bytes) =>
      Json.obj(
        "proofType" -> "Knowledge.VrfEd25519".asJson,
        "signature" -> bytes.data.asJson
      )
    case p: Proofs.Knowledge.KesProduct =>
      Json.obj(
        "proofType" -> "Knowledge.KesProduct".asJson,
        "signature" -> p.immutableBytes.asJson
      )
    case p: Proofs.Knowledge.KesSum =>
      Json.obj(
        "proofType" -> "Knowledge.KesSum".asJson,
        "signature" -> p.immutableBytes.asJson
      )
    case Proofs.Knowledge.HashLock(value) =>
      Json.obj(
        "proofType" -> "Knowledge.HashLock".asJson,
        "value"     -> value.asJson
      )
    case Proofs.Compositional.Threshold(proofs) =>
      Json.obj(
        "proofType" -> "Compositional.Threshold".asJson,
        "proofs"    -> proofs.asJson(Encoder.encodeList(proofEncoder))
      )
    case Proofs.Compositional.And(a, b) =>
      Json.obj(
        "proofType" -> "Compositional.And".asJson,
        "a"         -> a.asJson(proofEncoder),
        "b"         -> b.asJson(proofEncoder)
      )
    case Proofs.Compositional.Or(a, b) =>
      Json.obj(
        "proofType" -> "Compositional.Or".asJson,
        "a"         -> a.asJson(proofEncoder),
        "b"         -> b.asJson(proofEncoder)
      )
    case Proofs.Compositional.Not(a) =>
      Json.obj(
        "proofType" -> "Compositional.Not".asJson,
        "a"         -> a.asJson(proofEncoder)
      )
    case Proofs.Contextual.HeightLock() =>
      Json.obj(
        "proofType" -> "Contextual.HeightLock".asJson
      )
    case Proofs.Contextual.RequiredTransactionIO() =>
      Json.obj(
        "proofType" -> "Contextual.RequiredTransactionIO".asJson
      )
  }

  implicit val proofsKnowledgeCurve25519Decoder: Decoder[Proofs.Knowledge.Curve25519] = deriveDecoder

  implicit val proofsKnowledgeCurveEd25519Decoder: Decoder[Proofs.Knowledge.Ed25519] = deriveDecoder

  implicit def proofDecoder: Decoder[Proof] =
    hcursor =>
      hcursor.downField("proofType").as[String].flatMap {
        case "False"                => Proofs.Undefined.asRight[DecodingFailure]
        case "Knowledge.Curve25519" => hcursor.downField("signature").as[Proofs.Knowledge.Curve25519]
        case "Knowledge.Ed25519"    => hcursor.downField("signature").as[Proofs.Knowledge.Ed25519]
        case "Compositional.Threshold" =>
          hcursor
            .downField("proofs")
            .as(Decoder.decodeList(proofDecoder))
            .map(Proofs.Compositional.Threshold.apply)
        case "Compositional.And" =>
          (hcursor.downField("a").as(proofDecoder), hcursor.downField("b").as(proofDecoder))
            .mapN((a, b) => Proofs.Compositional.And(a, b))
        case "Compositional.Or" =>
          (hcursor.downField("a").as(proofDecoder), hcursor.downField("b").as(proofDecoder))
            .mapN((a, b) => Proofs.Compositional.Or(a, b))
        case "Contextual.HeightLock" => Proofs.Contextual.HeightLock().asRight
      }

  implicit val int128Codec: Encoder[Int128] =
    t => t.data.toString.asJson

  implicit val int128Decoder: Decoder[Int128] =
    Decoder[BigInt].flatMap(value =>
      Sized
        .max[BigInt, Lengths.`128`.type](value)
        .fold(error => Decoder.failedWithMessage(s"invalid int128: $error"), int128 => Decoder.const(int128))
    )

  implicit val latin1DataEncoder: Encoder[Latin1Data] =
    _.value.asJson

  implicit val latin1DataDecoder: Decoder[Latin1Data] =
    h => h.as[String].flatMap(Latin1Data.validated(_).toEither.leftMap(e => DecodingFailure(e.toString, Nil)))

  implicit val emptyBoxValueEncoder: Encoder[Box.Values.Empty.type] =
    _ => Json.Null

  implicit val polyBoxValueEncoder: Encoder[Box.Values.Poly] =
    t => Json.obj("value" -> t.quantity.asJson)

  implicit val polyBoxValueDecoder: Decoder[Box.Values.Poly] =
    hcursor => hcursor.downField("value").as[Int128].map(Box.Values.Poly)

  implicit val arbitBoxValueEncoder: Encoder[Box.Values.Arbit] =
    t => Json.obj("value" -> t.quantity.asJson)

  implicit val arbitBoxValueDecoder: Decoder[Box.Values.Arbit] =
    hcursor => hcursor.downField("value").as[Int128].map(Box.Values.Arbit)

  implicit val assetCodeEncoder: Encoder[Box.Values.Asset.Code] =
    t =>
      Json.obj(
        "version"   -> t.version.asJson,
        "issuer"    -> t.issuer.asJson,
        "shortName" -> t.shortName.data.value.asJson
      )

  implicit val assetCodeDecoder: Decoder[Box.Values.Asset.Code] =
    hcursor =>
      for {
        version   <- hcursor.downField("version").as[Byte]
        issuer    <- hcursor.downField("issuer").as[SpendingAddress]
        shortName <- hcursor.downField("shortName").as[Latin1Data]
        validLengthShortName <- Sized
          .max[Latin1Data, Lengths.`8`.type](shortName)
          .leftMap(failure => DecodingFailure(failure.toString, List(CursorOp.Field("shortName"))))
      } yield Box.Values.Asset.Code(version, issuer, validLengthShortName)

  implicit val assetBoxValueEncoder: Encoder[Box.Values.Asset] =
    t =>
      Json.obj(
        "quantity"     -> t.quantity.asJson,
        "assetCode"    -> t.assetCode.asJson,
        "securityRoot" -> t.securityRoot.asJson,
        "metadata"     -> t.metadata.map(_.data).asJson
      )

  implicit val assetBoxValueDecoder: Decoder[Box.Values.Asset] =
    deriveDecoder

  implicit val baseRegistrationBoxValueEncoder: Encoder[Box.Values.Registrations.Operator] =
    t => Json.obj("vrfCommitment" -> t.vrfCommitment.immutableBytes.asJson)

  implicit val taktikosRegistrationBoxValueDecoder: Decoder[Box.Values.Registrations.Operator] =
    h =>
      h.downField("vrfCommitment")
        .as[Bytes]
        .flatMap(_.decodeImmutable[Proofs.Knowledge.KesProduct].leftMap(e => DecodingFailure(e, Nil)))
        .map(Box.Values.Registrations.Operator)

  def boxValueTypeName(value: Box.Value): String =
    value match {
      case Box.Values.Empty                     => "Empty"
      case _: Box.Values.Poly                   => "Poly"
      case _: Box.Values.Arbit                  => "Arbit"
      case _: Box.Values.Asset                  => "Asset"
      case _: Box.Values.Registrations.Operator => "Registrations.Operator"
    }

  implicit val boxValueEncoder: Encoder[Box.Value] = {
    case Box.Values.Empty                     => Json.Null
    case v: Box.Values.Poly                   => v.asJson
    case v: Box.Values.Arbit                  => v.asJson
    case v: Box.Values.Asset                  => v.asJson
    case v: Box.Values.Registrations.Operator => v.asJson
  }

  implicit val boxEncoder: Encoder[Box] =
    t =>
      Json.obj(
        "evidence"  -> t.evidence.asJson,
        "valueType" -> boxValueTypeName(t.value).asJson,
        "value"     -> t.value.asJson
      )

  implicit val encodeBoxId: Encoder[Box.Id] =
    t =>
      Json.obj(
        "transactionId"          -> t.transactionId.asJson,
        "transactionOutputIndex" -> t.transactionOutputIndex.asJson
      )

  implicit val encodeTransactionInput: Encoder[Transaction.Input] =
    input =>
      Json.obj(
        "boxId"       -> input.boxId.asJson,
        "proposition" -> input.proposition.asJson,
        "proof"       -> input.proof.asJson,
        "valueType"   -> boxValueTypeName(input.value).asJson,
        "value"       -> input.value.asJson
      )

  implicit val decodeBoxId: Decoder[Box.Id] =
    hcursor =>
      for {
        transactionId          <- hcursor.downField("transactionId").as[TypedIdentifier]
        transactionOutputIndex <- hcursor.downField("transactionOutputIndex").as[Short]
      } yield Box.Id(transactionId, transactionOutputIndex)

  implicit val decodeTransactionInput: Decoder[Transaction.Input] =
    hcursor =>
      for {
        boxId       <- hcursor.downField("boxId").as[Box.Id]
        proposition <- hcursor.downField("proposition").as[Proposition]
        proof       <- hcursor.downField("proof").as[Proof]
        valueType   <- hcursor.downField("valueType").as[String]
        valueJson = hcursor.downField("value")
        value <- valueType match {
          case "Poly"                  => valueJson.as[Box.Values.Poly]
          case "Arbit"                 => valueJson.as[Box.Values.Arbit]
          case "Asset"                 => valueJson.as[Box.Values.Asset]
          case "Registration.Operator" => valueJson.as[Box.Values.Registrations.Operator]
        }
      } yield Transaction.Input(boxId, proposition, proof, value)

  implicit val encodeTransactionUnprovenInput: Encoder[Transaction.Unproven.Input] =
    input =>
      Json.obj(
        "boxId"       -> input.boxId.asJson,
        "proposition" -> input.proposition.asJson,
        "valueType"   -> boxValueTypeName(input.value).asJson,
        "value"       -> input.value.asJson
      )

  implicit val decodeTransactionUnprovenInput: Decoder[Transaction.Unproven.Input] =
    hcursor =>
      for {
        boxId       <- hcursor.downField("boxId").as[Box.Id]
        proposition <- hcursor.downField("proposition").as[Proposition]
        valueType   <- hcursor.downField("valueType").as[String]
        valueJson = hcursor.downField("value")
        value <- valueType match {
          case "Poly"              => valueJson.as[Box.Values.Poly]
          case "Arbit"             => valueJson.as[Box.Values.Arbit]
          case "Asset"             => valueJson.as[Box.Values.Asset]
          case "Registration.Pool" => valueJson.as[Box.Values.Registrations.Operator]
        }
      } yield Transaction.Unproven.Input(boxId, proposition, value)

  implicit val encodeTransactionOutput: Encoder[Transaction.Output] =
    o =>
      Json.obj(
        "address"   -> o.address.asJson,
        "valueType" -> boxValueTypeName(o.value).asJson,
        "value"     -> o.value.asJson,
        "minting"   -> o.minting.asJson
      )

  implicit val decodeTransactionOutput: Decoder[Transaction.Output] =
    hcursor =>
      for {
        address   <- hcursor.downField("address").as[FullAddress]
        valueType <- hcursor.downField("valueType").as[String]
        minting   <- hcursor.downField("minting").as[Boolean]
        valueJson = hcursor.downField("value")
        output <- valueType match {
          case "Poly" =>
            valueJson.as[Box.Values.Poly].map(value => Transaction.Output(address, value, minting))
          case "Arbit" =>
            valueJson.as[Box.Values.Arbit].map(value => Transaction.Output(address, value, minting))
          case "Asset" =>
            valueJson.as[Box.Values.Asset].map(value => Transaction.Output(address, value, minting))
          case "Registration.Pool" =>
            valueJson.as[Box.Values.Registrations.Operator].map(value => Transaction.Output(address, value, minting))
          case _ =>
            DecodingFailure("invalid output type", List(CursorOp.Field("valueType"))).asLeft
        }
      } yield output

  implicit val transactionChronologyJsonEncoder: Encoder[Transaction.Chronology] =
    t =>
      Json.obj(
        "creation"    -> t.creation.asJson,
        "minimumSlot" -> t.minimumSlot.asJson,
        "maximumSlot" -> t.maximumSlot.asJson
      )

  implicit val transactionChronologyJsonDecoder: Decoder[Transaction.Chronology] =
    deriveDecoder

  implicit val transactionJsonEncoder: Encoder[Transaction] =
    tx =>
      Json.obj(
        "inputs"     -> tx.inputs.asJson,
        "outputs"    -> tx.outputs.asJson,
        "chronology" -> tx.chronology.asJson,
        "data"       -> tx.data.map(_.data).asJson
      )

  implicit val transactionJsonDecoder: Decoder[Transaction] =
    hcursor =>
      for {
        inputs     <- hcursor.downField("inputs").as[Chain[Transaction.Input]]
        outputs    <- hcursor.downField("outputs").as[Chain[Transaction.Output]]
        chronology <- hcursor.downField("chronology").as[Transaction.Chronology]
        data       <- hcursor.downField("data").as[Option[TransactionData]]
      } yield Transaction(inputs, outputs, chronology, data)

  implicit val unprovenTransactionJsonEncoder: Encoder[Transaction.Unproven] =
    tx =>
      Json.obj(
        "inputs"     -> tx.inputs.asJson,
        "outputs"    -> tx.outputs.asJson,
        "chronology" -> tx.chronology.asJson,
        "data"       -> tx.data.map(_.data).asJson
      )

  implicit val unprovenTransactionJsonDecoder: Decoder[Transaction.Unproven] =
    hcursor =>
      for {
        inputs     <- hcursor.downField("inputs").as[Chain[Transaction.Unproven.Input]]
        outputs    <- hcursor.downField("outputs").as[Chain[Transaction.Output]]
        chronology <- hcursor.downField("chronology").as[Transaction.Chronology]
        data       <- hcursor.downField("data").as[Option[TransactionData]]
      } yield Transaction.Unproven(inputs, outputs, chronology, data)

}
