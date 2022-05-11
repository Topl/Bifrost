package co.topl.codecs.json.tetra

import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility.StringDataTypes.Latin1Data
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}

trait ModelsJsonCodecs {

  implicit val bytesEncoder: Encoder[Bytes] =
    _.toBase58.asJson

  implicit val typedBytesEncoder: Encoder[TypedBytes] =
    bytes => bytes.allBytes.asJson

  implicit val typedEvidenceEncoder: Encoder[TypedEvidence] =
    bytes => bytes.allBytes.asJson

  implicit val dionAddressEncoder: Encoder[DionAddress] =
    t => t.allBytes.asJson

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
        "propositionType" -> "Contextual.RequiredBoxState".asJson,
        "location" -> (location match {
          case BoxLocations.Input  => "input"
          case BoxLocations.Output => "output"
        }).asJson,
        "boxes" -> boxes.map { case (i, b) => List(i.asJson, b.asJson).asJson }.asJson
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
    case Proofs.Knowledge.HashLock(salt, value) =>
      Json.obj(
        "proofType" -> "Knowledge.HashLock".asJson,
        "salt"      -> salt.data.asJson,
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
    case Proofs.Contextual.RequiredBoxState() =>
      Json.obj(
        "proofType" -> "Contextual.RequiredBoxState".asJson
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

  implicit val emptyBoxValueEncoder: Encoder[Box.Values.Empty.type] =
    _ => Json.Null

  implicit val polyBoxValueEncoder: Encoder[Box.Values.Poly] =
    t => Json.obj("value" -> t.value.asJson)

  implicit val arbitBoxValueEncoder: Encoder[Box.Values.Arbit] =
    t => Json.obj("value" -> t.value.asJson)

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
        "securityRoot" -> t.securityRoot.asJson,
        "metadata"     -> t.metadata.map(_.data).asJson
      )

  implicit val taktikosRegistrationBoxValueEncoder: Encoder[Box.Values.TaktikosRegistration] =
    t => Json.obj("commitment" -> t.commitment.immutableBytes.asJson)

  def boxValueTypeName(value: Box.Value): String =
    value match {
      case Box.Values.Empty                   => "Empty"
      case _: Box.Values.Poly                 => "Poly"
      case _: Box.Values.Arbit                => "Arbit"
      case _: Box.Values.Asset                => "Asset"
      case _: Box.Values.TaktikosRegistration => "TaktikosRegistration"
    }

  implicit val boxValueEncoder: Encoder[Box.Value] = {
    case Box.Values.Empty                   => Json.Null
    case v: Box.Values.Poly                 => v.asJson
    case v: Box.Values.Arbit                => v.asJson
    case v: Box.Values.Asset                => v.asJson
    case v: Box.Values.TaktikosRegistration => v.asJson
  }

  implicit val boxEncoder: Encoder[Box] =
    t =>
      Json.obj(
        "evidence"  -> t.evidence.asJson,
        "valueType" -> boxValueTypeName(t.value).asJson,
        "value"     -> t.value.asJson
      )

  implicit val encodeTransactionInput: Encoder[Transaction.Input] =
    input =>
      Json.obj(
        "transactionId"          -> input.transactionId.asJson,
        "transactionOutputIndex" -> input.transactionOutputIndex.asJson,
        "proposition"            -> input.proposition.asJson,
        "proof"                  -> input.proof.asJson,
        "valueType"              -> boxValueTypeName(input.value).asJson,
        "value"                  -> input.value.asJson
      )

  implicit val encodeTransactionOutput: Encoder[Transaction.Output] =
    o =>
      Json.obj(
        "address"   -> o.dionAddress.asJson,
        "valueType" -> boxValueTypeName(o.value).asJson,
        "value"     -> o.value.asJson,
        "minting"   -> o.minting.asJson
      )

  implicit val transactionJsonEncoder: Encoder[Transaction] =
    tx =>
      Json.obj(
        "inputs"    -> tx.inputs.asJson,
        "outputs"   -> tx.outputs.asJson,
        "timestamp" -> tx.timestamp.asJson,
        "data"      -> tx.data.map(_.data).asJson
      )

}
