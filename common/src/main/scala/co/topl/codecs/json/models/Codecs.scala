package co.topl.codecs.json.models

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models._
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

trait Codecs {

  implicit val dionAddressEncoder: Encoder[DionAddress] =
    t => t.allBytes.toBase58.asJson

  implicit def propositionEncoder: Encoder[Proposition] = {
    case Propositions.PermanentlyLocked =>
      Json.obj("propositionType" -> "PermanentlyLocked".asJson)
    case Propositions.Knowledge.Curve25519(k) =>
      Json.obj("propositionType" -> "Knowledge.Curve25519".asJson, "key" -> k.bytes.data.toBase58.asJson)
    case Propositions.Knowledge.Ed25519(k) =>
      Json.obj("propositionType" -> "Knowledge.Ed25519".asJson, "key" -> k.bytes.data.toBase58.asJson)
    case Propositions.Knowledge.ExtendedEd25519(k) =>
      Json.obj(
        "propositionType" -> "Knowledge.ExtendedEd25519".asJson,
        "key" -> Json.obj(
          "vk"        -> k.vk.bytes.data.toBase58.asJson,
          "chainCode" -> k.chainCode.data.toBase58.asJson
        )
      )
    case Propositions.Compositional.Threshold(threshold, propositions) =>
      Json.obj(
        "propositionType" -> "Compositional.Threshold".asJson,
        "threshold"       -> threshold.asJson,
        "propositions"    -> propositions.toList.map(_.asJson(propositionEncoder)).asJson
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
    case Propositions.Contextual.HeightLock(height) =>
      Json.obj(
        "propositionType" -> "Contextual.HeightLock".asJson,
        "height"          -> height.asJson
      )
    case Propositions.Script.JS(script) =>
      Json.obj(
        "propositionType" -> "Script.JS".asJson,
        "script"          -> script.value.asJson
      )
  }

  implicit def proofEncoder: Encoder[Proof] = {
    case Proofs.False =>
      Json.obj(
        "proofType" -> "False".asJson
      )
    case Proofs.Knowledge.Curve25519(bytes) =>
      Json.obj(
        "proofType" -> "Knowledge.Curve25519".asJson,
        "signature" -> bytes.data.toBase58.asJson
      )
    case Proofs.Knowledge.Ed25519(bytes) =>
      Json.obj(
        "proofType" -> "Knowledge.Ed25519".asJson,
        "signature" -> bytes.data.toBase58.asJson
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

  implicit val int128Codec: Encoder[Int128] =
    t => t.data.toString.asJson

  implicit val latin1DataEncoder: Encoder[Latin1Data] =
    _.value.asJson

  implicit val assetCodeEncoder: Encoder[Box.Values.Asset.Code] =
    t =>
      Json.obj(
        "version"   -> t.version.asJson,
        "issuer"    -> t.issuer.asJson,
        "shortName" -> t.shortName.data.value.asJson
      )

  implicit val assetBoxValueEncoder: Encoder[Box.Values.Asset] =
    t =>
      Json.obj(
        "quantity"     -> t.quantity.asJson,
        "assetCode"    -> t.assetCode.asJson,
        "securityRoot" -> t.securityRoot.toBase58.asJson,
        "metadata"     -> t.metadata.map(_.data).asJson
      )

  implicit val encodeCoinOutput: Encoder[Transaction.CoinOutput] = {
    case o: Transaction.PolyOutput =>
      Json.obj(
        "coinType" -> "Poly".asJson,
        "address"  -> o.dionAddress.asJson,
        "value"    -> o.value.asJson
      )
    case o: Transaction.ArbitOutput =>
      Json.obj(
        "coinType" -> "Arbit".asJson,
        "address"  -> o.dionAddress.asJson,
        "value"    -> o.value.asJson
      )
    case o: Transaction.AssetOutput =>
      Json.obj(
        "coinType" -> "Asset".asJson,
        "address"  -> o.dionAddress.asJson,
        "value"    -> o.value.asJson
      )
  }

  implicit val transactionJsonEncoder: Encoder[Transaction] =
    tx =>
      Json.obj(
        "inputs" -> tx.inputs.map { case (boxRef, (prop, proof)) =>
          Json.obj(
            "box" -> Json.obj(
              "address" -> boxRef._1.asJson,
              "nonce"   -> boxRef._2.asJson
            ),
            "proposition" -> prop.asJson,
            "proof"       -> proof.asJson
          )
        }.asJson,
        "feeOutput"   -> (tx.feeOutput: Option[Transaction.CoinOutput]).asJson,
        "coinOutputs" -> tx.coinOutputs.toNonEmptyList.toList.asJson,
        "fee"         -> tx.fee.asJson,
        "timestamp"   -> tx.timestamp.asJson,
        "data"        -> tx.data.map(_.data).asJson,
        "minting"     -> tx.minting.asJson
      )

}
