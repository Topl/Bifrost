package co.topl.codecs.json.tetra

import cats.data.{NonEmptyChain, NonEmptyList}
import cats.implicits._
import co.topl.models.Propositions.Contextual.HeightLock
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{HasLength, Length, Lengths, Sized}
import io.circe._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps

import scala.collection.immutable.{ListMap, ListSet}

trait ModelsJsonCodecs {

  implicit val bytesEncoder: Encoder[Bytes] = Encoder[String].contramap(_.toBase58)

  implicit val bytesDecoder: Decoder[Bytes] =
    Decoder[String].flatMap(string =>
      Bytes
        .fromBase58(string)
        .fold(Decoder.failedWithMessage[Bytes]("invalid base-58"))(bytes => Decoder.const(bytes))
    )

  implicit def listSetEncoder[T: Encoder]: Encoder[ListSet[T]] = Encoder[List[T]].contramap(_.toList)

  implicit def listSetDecoder[T: Decoder]: Decoder[ListSet[T]] = Decoder[List[T]].map(list => ListSet(list: _*))

  implicit def nonEmptyChainEncoder[T: Encoder]: Encoder[NonEmptyChain[T]] =
    Encoder[NonEmptyList[T]].contramap(_.toNonEmptyList)

  implicit def nonEmptyChainDecoder[T: Decoder]: Decoder[NonEmptyChain[T]] =
    Decoder[NonEmptyList[T]].map(NonEmptyChain.fromNonEmptyList)

  implicit def sizedMaxEncoder[T: Encoder, L]: Encoder[Sized.Max[T, L]] = Encoder[T].contramap(_.data)

  implicit def sizedMaxDecoder[T: Decoder: HasLength, L <: Length](implicit
    length: L
  ): Decoder[Sized.Max[T, L]] =
    cursor =>
      for {
        value <- cursor.as[T]
        sized <- Sized
          .max[T, L](value)
          .leftMap(failure => DecodingFailure(s"invalid value $value with length ${failure.length}", List.empty))
      } yield sized

  implicit def sizedStrictEncoder[T: Encoder, L]: Encoder[Sized.Strict[T, L]] = Encoder[T].contramap(_.data)

  implicit def sizedStrictDecoder[T: Decoder: HasLength, L <: Length](implicit length: L): Decoder[Sized.Strict[T, L]] =
    cursor =>
      for {
        value <- cursor.as[T]
        sized <- Sized
          .strict[T, L](value)
          .leftMap(failure => DecodingFailure(s"invalid value $value with length ${failure.length}", List.empty))
      } yield sized

  implicit val networkPrefixEncoder: Encoder[NetworkPrefix] = Encoder[Byte].contramap(_.value)
  implicit val networkPrefixDecoder: Decoder[NetworkPrefix] = Decoder[Byte].map(byte => NetworkPrefix(byte))

  implicit val dionAddressEncoder: Encoder[DionAddress] =
    t => t.allBytes.toBase58.asJson

  implicit val dionAddressDecoder: Decoder[DionAddress] =
    for {
      string <- Decoder[String]
      bytes <-
        Bytes
          .fromBase58(string)
          .fold(Decoder.failedWithMessage[Bytes]("address is an invalid base-58 string"))(Decoder.const)
      networkPrefix <-
        bytes.headOption
          .fold(Decoder.failedWithMessage[NetworkPrefix]("address length is too short"))(byte =>
            Decoder.const(NetworkPrefix(byte))
          )
      evidenceTypePrefix <-
        bytes.tail.headOption
          .fold(Decoder.failedWithMessage[TypePrefix]("address length is too short"))(byte => Decoder.const(byte))
      evidence <-
        Sized
          .strict[Bytes, Lengths.`32`.type](bytes.tail.tail)
          .fold(error => Decoder.failedWithMessage(error.toString), evidence => Decoder.const(evidence))
    } yield DionAddress(networkPrefix, TypedEvidence(evidenceTypePrefix, evidence))

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
      Json.obj("propositionType" -> "Knowledge.Curve25519".asJson, "key" -> k.asJson)
    case Propositions.Knowledge.Ed25519(k) =>
      Json.obj("propositionType" -> "Knowledge.Ed25519".asJson, "key" -> k.asJson)
    case Propositions.Knowledge.ExtendedEd25519(k) =>
      Json.obj(
        "propositionType" -> "Knowledge.ExtendedEd25519".asJson,
        "key"             -> k.asJson
      )
    case Propositions.Knowledge.HashLock(d) =>
      Json.obj(
        "propositionType" -> "Knowledge.HashLock".asJson,
        "digest"          -> d.asJson
      )
    case Propositions.Compositional.Threshold(threshold, propositions) =>
      Json.obj(
        "propositionType" -> "Compositional.Threshold".asJson,
        "threshold"       -> threshold.asJson,
        "propositions"    -> propositions.asJson(listSetEncoder(propositionEncoder))
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
    case Propositions.Contextual.RequiredBoxState(location, boxes) =>
      Json.obj(
        "propositionType" -> "Contextual.RequiredBoxState".asJson
      )
    case Propositions.Script.JS(script) =>
      Json.obj(
        "propositionType" -> "Script.JS".asJson,
        "script"          -> script.value.asJson
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
              .as[ListSet[Proposition]](listSetDecoder(propositionDecoder))
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
        case "Script.JS" =>
          hcursor
            .downField("script")
            .as[String]
            .map(Propositions.Script.JS.JSScript.apply)
            .map(Propositions.Script.JS.apply)
      }

  implicit def proofEncoder: Encoder[Proof] = {
    case Proofs.False =>
      Json.obj(
        "proofType" -> "False".asJson
      )
    case Proofs.Knowledge.Curve25519(bytes) =>
      Json.obj(
        "proofType" -> "Knowledge.Curve25519".asJson,
        "signature" -> bytes.asJson
      )
    case Proofs.Knowledge.Ed25519(bytes) =>
      Json.obj(
        "proofType" -> "Knowledge.Ed25519".asJson,
        "signature" -> bytes.asJson
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
    case Proofs.Contextual.HeightLock() =>
      Json.obj(
        "proofType" -> "Contextual.HeightLock".asJson
      )
    case Proofs.Script.JS(serializedArgs) =>
      Json.obj(
        "proofType" -> "Script.JS".asJson,
        "args"      -> io.circe.parser.parse(serializedArgs).getOrElse(serializedArgs.asJson)
      )
  }

  implicit val proofsKnowledgeCurve25519Decoder: Decoder[Proofs.Knowledge.Curve25519] = deriveDecoder

  implicit val proofsKnowledgeCurveEd25519Decoder: Decoder[Proofs.Knowledge.Ed25519] = deriveDecoder

  implicit def proofDecoder: Decoder[Proof] =
    hcursor =>
      hcursor.downField("proofType").as[String].flatMap {
        case "False"                => Proofs.False.asRight[DecodingFailure]
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
        case "Script.JS"             => hcursor.downField("args").as[String].map(Proofs.Script.JS.apply)
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
    Decoder[String]
      .flatMap(string =>
        Latin1Data
          .validated(string)
          .fold(failure => Decoder.failedWithMessage(failure.toString), data => Decoder.const(data))
      )

  implicit val transactionDataEncoder: Encoder[TransactionData] =
    Encoder[Latin1Data].contramap(transactionData => transactionData.data)

  implicit val transactionDataDecoder: Decoder[TransactionData] = sizedMaxDecoder[Latin1Data, Lengths.`127`.type]

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
        issuer    <- hcursor.downField("issuer").as[DionAddress]
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
        "securityRoot" -> t.securityRoot.toBase58.asJson,
        "metadata"     -> t.metadata.map(_.data).asJson
      )

  implicit val assetBoxValueDecoder: Decoder[Box.Values.Asset] =
    hcursor =>
      for {
        quantity           <- hcursor.downField("quantity").as[Int128]
        assetCode          <- hcursor.downField("assetCode").as[Box.Values.Asset.Code]
        securityRootString <- hcursor.downField("securityRoot").as[String]
        securityRootBytes <- Bytes
          .fromBase58(securityRootString)
          .toRight(DecodingFailure("invalid base-58 data", List(CursorOp.Field("securityRoot"))))
        metadata <- hcursor.downField("metadata").as[Option[Sized.Max[Latin1Data, Lengths.`127`.type]]]
      } yield Box.Values.Asset(quantity, assetCode, securityRootBytes, metadata)

  implicit val polyOutputEncoder: Encoder[Transaction.PolyOutput] =
    t =>
      Json.obj(
        "dionAddress" -> t.dionAddress.asJson,
        "value"       -> t.value.asJson
      )

  implicit val polyOutputDecoder: Decoder[Transaction.PolyOutput] =
    hcursor =>
      for {
        address <- hcursor.downField("dionAddress").as[DionAddress]
        value   <- hcursor.downField("value").as[Int128]
      } yield Transaction.PolyOutput(address, value)

  implicit val encodeCoinOutput: Encoder[Transaction.CoinOutput] = {
    case o: Transaction.PolyOutput =>
      Json.obj(
        "coinType" -> "Poly".asJson,
        "address"  -> o.dionAddress.asJson,
        "value"    -> o.value.asJson
      )
    case o: Transaction.ArbitOutput =>
      Json.obj(
        "coinType"    -> "Arbit".asJson,
        "dionAddress" -> o.dionAddress.asJson,
        "value"       -> o.value.asJson
      )
    case o: Transaction.AssetOutput =>
      Json.obj(
        "coinType" -> "Asset".asJson,
        "address"  -> o.dionAddress.asJson,
        "value"    -> o.value.asJson
      )
  }

  implicit val decodeCoinOutput: Decoder[Transaction.CoinOutput] =
    hcursor =>
      for {
        address  <- hcursor.downField("address").as[DionAddress]
        coinType <- hcursor.downField("coinType").as[String]
        valueJson = hcursor.downField("value")
        output <- coinType match {
          case "Poly" =>
            valueJson.as[Int128].map(value => Transaction.PolyOutput(address, value))
          case "Arbit" =>
            valueJson.as[Int128].map(value => Transaction.PolyOutput(address, value))
          case "Asset" =>
            valueJson.as[Box.Values.Asset].map(value => Transaction.AssetOutput(address, value))
          case _ =>
            DecodingFailure("invalid coin type", List(CursorOp.Field("coinType"))).asLeft
        }
      } yield output

  implicit val boxReferenceEncoder: Encoder[BoxReference] =
    t =>
      Json.obj(
        "address" -> t._1.asJson,
        "nonce"   -> t._2.asJson
      )

  implicit val boxReferenceDecoder: Decoder[BoxReference] =
    hcursor =>
      for {
        address <- hcursor.downField("address").as[DionAddress]
        nonce   <- hcursor.downField("nonce").as[BoxNonce]
      } yield address -> nonce

  implicit val transactionJsonEncoder: Encoder[Transaction] =
    tx =>
      Json.obj(
        "inputs"      -> tx.inputs.toList.asJson,
        "feeOutput"   -> tx.feeOutput.asJson,
        "coinOutputs" -> tx.coinOutputs.asJson(nonEmptyChainEncoder[Transaction.CoinOutput]),
        "fee"         -> tx.fee.asJson,
        "timestamp"   -> tx.timestamp.asJson,
        "data"        -> tx.data.asJson,
        "minting"     -> tx.minting.asJson
      )

  implicit val transactionJsonDecoder: Decoder[Transaction] =
    hcursor =>
      for {
        inputs <- hcursor
          .downField("inputs")
          .as[List[(BoxReference, (Proposition, Proof))]]
          .map(list => ListMap(list: _*))
        feeOutput   <- hcursor.downField("feeOutput").as[Option[Transaction.PolyOutput]]
        coinOutputs <- hcursor.downField("coinOutputs").as[NonEmptyChain[Transaction.CoinOutput]]
        fee         <- hcursor.downField("fee").as[Int128]
        timestamp   <- hcursor.downField("timestamp").as[Timestamp]
        data        <- hcursor.downField("data").as[Option[TransactionData]]
        minting     <- hcursor.downField("minting").as[Boolean]
      } yield Transaction(inputs, feeOutput, coinOutputs, fee, timestamp, data, minting)

  implicit val unprovenTransactionJsonEncoder: Encoder[Transaction.Unproven] = deriveEncoder

  implicit val unprovenTransactionJsonDecoder: Decoder[Transaction.Unproven] = deriveDecoder

}
