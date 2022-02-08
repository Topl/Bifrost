package co.topl.codecs.json.modifier.transaction

import cats.implicits._
import co.topl.attestation._
import co.topl.codecs.binary._
import co.topl.codecs.json.modifier.box._
import co.topl.codecs.json.valuetypes.ValueTypesJsonCodecs
import co.topl.modifier.box.{Box, SimpleValue, TokenValueHolder}
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction.TransferTransaction.encodeFrom
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import scodec.bits.BitVector

import scala.collection.immutable.ListMap

trait TransactionJsonCodecs extends ValueTypesJsonCodecs {

  implicit val publicKeyCurve25519AttestationJsonDecoder
    : Decoder[ListMap[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]] =
    Decoder[ListMap[Base58Data, Base58Data]]
      .emap(attestation =>
        attestation.foldLeft(
          ListMap[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]()
            .asRight[String]
        ) {
          case (Right(signatures), (key, value)) =>
            (for {
              publicKey <-
                scodec
                  .Decoder[PublicKeyPropositionCurve25519]
                  // We are assuming here that the first byte in the key will be a proposition type byte.
                  // If the first byte is not a proposition type byte, then this will fail or yield an incorrect result.
                  .decodeValue(BitVector(key.encodeAsBytes.tail))
                  .toEither
                  .leftMap(err => s"failed to decode public key curve 25519 proposition from binary data: $err")
              sig <-
                scodec
                  .Decoder[SignatureCurve25519]
                  // We are assuming here that the first byte in the key will be a proof type byte.
                  // If the first byte is not a proof type byte, then this will fail or yield an incorrect result.
                  .decodeValue(BitVector(value.encodeAsBytes.tail))
                  .toEither
                  .leftMap(err => s"failed to decode curve 25519 signature from binary data: $err")
            } yield publicKey -> sig)
              .map(pair => signatures ++ ListMap(pair))
          case (err, _) => err
        }
      )

  implicit val thresholdCurve25519AttestationJsonDecoder
    : Decoder[ListMap[ThresholdPropositionCurve25519, Proof[ThresholdPropositionCurve25519]]] =
    Decoder[ListMap[Base58Data, Base58Data]]
      .emap(attestation =>
        attestation.foldLeft(
          ListMap[ThresholdPropositionCurve25519, Proof[ThresholdPropositionCurve25519]]()
            .asRight[String]
        ) {
          case (Right(signatures), (key, value)) =>
            (for {
              publicKey <-
                scodec
                  .Decoder[ThresholdPropositionCurve25519]
                  // We are assuming here that the first byte in the key will be a proposition type byte.
                  // If the first byte is not a proposition type byte, then this will fail or yield an incorrect result.
                  .decodeValue(BitVector(key.encodeAsBytes.tail))
                  .toEither
                  .leftMap(err => s"failed to decode threshold curve 25519 proposition from binary data: $err")
              sig <-
                scodec
                  .Decoder[ThresholdSignatureCurve25519]
                  // We are assuming here that the first byte in the key will be a proof type byte.
                  // If the first byte is not a proof type byte, then this will fail or yield an incorrect result.
                  .decodeValue(BitVector(value.encodeAsBytes.tail))
                  .toEither
                  .leftMap(err => s"failed to decode threshold curve 25519 signature from binary data: $err")
            } yield publicKey -> sig)
              .map(pair => signatures ++ ListMap(pair))
          case (err, _) => err
        }
      )

  implicit val publicKeyEd25519AttestationJsonDecoder
    : Decoder[ListMap[PublicKeyPropositionEd25519, Proof[PublicKeyPropositionEd25519]]] =
    Decoder[ListMap[Base58Data, Base58Data]]
      .emap(attestation =>
        attestation.foldLeft(
          ListMap[PublicKeyPropositionEd25519, Proof[PublicKeyPropositionEd25519]]()
            .asRight[String]
        ) {
          case (Right(signatures), (key, value)) =>
            (for {
              publicKey <-
                scodec
                  .Decoder[PublicKeyPropositionEd25519]
                  // We are assuming here that the first byte in the key will be a proposition type byte.
                  // If the first byte is not a proposition type byte, then this will fail or yield an incorrect result.
                  .decodeValue(BitVector(key.encodeAsBytes.tail))
                  .toEither
                  .leftMap(err => s"failed to decode public key ed 25519 proposition from binary data: $err")
              sig <-
                scodec
                  .Decoder[SignatureEd25519]
                  // We are assuming here that the first byte in the key will be a proof type byte.
                  // If the first byte is not a proof type byte, then this will fail or yield an incorrect result.
                  .decodeValue(BitVector(value.encodeAsBytes.tail))
                  .toEither
                  .leftMap(err => s"failed to decode signature ed 25519 from binary data: $err")
            } yield publicKey -> sig)
              .map(pair => signatures ++ ListMap(pair))
          case (err, _) => err
        }
      )

  implicit def arbitTransferJsonEncoder[P <: Proposition]: Encoder[ArbitTransfer[P]] = { tx: ArbitTransfer[P] =>
    Map(
      "txId"            -> tx.id.asJson,
      "txType"          -> "ArbitTransfer".asJson,
      "propositionType" -> tx.getPropIdentifier.typeString.asJson,
      "newBoxes"        -> tx.newBoxes.toSeq.asJson,
      "boxesToRemove"   -> tx.boxIdsToOpen.asJson,
      "from"            -> encodeFrom(tx.from),
      "to"              -> tx.to.asJson,
      "signatures"      -> tx.attestation.asJson,
      "fee"             -> tx.fee.asJson,
      "timestamp"       -> tx.timestamp.asJson,
      "minting"         -> tx.minting.asJson,
      "data"            -> tx.data.asJson
    ).asJson
  }

  implicit def arbitTransferJsonDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[ArbitTransfer[_ <: Proposition]] =
    (c: HCursor) =>
      for {
        from      <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to        <- c.downField("to").as[IndexedSeq[(Address, SimpleValue)]]
        fee       <- c.get[Int128]("fee")
        timestamp <- c.downField("timestamp").as[Long]
        data      <- c.downField("data").as[Option[Latin1Data]]
        propType  <- c.downField("propositionType").as[String]
        minting   <- c.downField("minting").as[Boolean]
        transfer <- propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            c.downField("signatures")
              .as[ListMap[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]]
              .map(ArbitTransfer(from, to, _, fee, timestamp, data, minting))
          case ThresholdPropositionCurve25519.`typeString` =>
            c.downField("signatures")
              .as[ListMap[ThresholdPropositionCurve25519, Proof[ThresholdPropositionCurve25519]]]
              .map(ArbitTransfer(from, to, _, fee, timestamp, data, minting))
          case PublicKeyPropositionEd25519.`typeString` =>
            c.downField("signatures")
              .as[ListMap[PublicKeyPropositionEd25519, Proof[PublicKeyPropositionEd25519]]]
              .map(ArbitTransfer(from, to, _, fee, timestamp, data, minting))
        }
      } yield transfer

  implicit def assetTransferJsonEncoder[P <: Proposition]: Encoder[AssetTransfer[P]] = { tx: AssetTransfer[P] =>
    Map(
      "txId"            -> tx.id.asJson,
      "txType"          -> "AssetTransfer".asJson,
      "propositionType" -> tx.getPropIdentifier.typeString.asJson,
      "newBoxes"        -> tx.newBoxes.toSeq.asJson,
      "boxesToRemove"   -> tx.boxIdsToOpen.asJson,
      "from"            -> encodeFrom(tx.from),
      "to"              -> tx.to.asJson,
      "signatures"      -> tx.attestation.asJson,
      "fee"             -> tx.fee.asJson,
      "timestamp"       -> tx.timestamp.asJson,
      "data"            -> tx.data.asJson,
      "minting"         -> tx.minting.asJson
    ).asJson
  }

  implicit def assetTransferJsonDecoder(implicit
    networkPrefix: NetworkPrefix
  ): Decoder[AssetTransfer[_ <: Proposition]] =
    (c: HCursor) =>
      for {
        from      <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to        <- c.downField("to").as[IndexedSeq[(Address, TokenValueHolder)]]
        fee       <- c.get[Int128]("fee")
        timestamp <- c.downField("timestamp").as[Long]
        data      <- c.downField("data").as[Option[Latin1Data]]
        minting   <- c.downField("minting").as[Boolean]
        propType  <- c.downField("propositionType").as[String]
        transfer <- propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            c.downField("signatures")
              .as[ListMap[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]]
              .map(AssetTransfer(from, to, _, fee, timestamp, data, minting))
          case ThresholdPropositionCurve25519.`typeString` =>
            c.downField("signatures")
              .as[ListMap[ThresholdPropositionCurve25519, Proof[ThresholdPropositionCurve25519]]]
              .map(AssetTransfer(from, to, _, fee, timestamp, data, minting))
          case PublicKeyPropositionEd25519.`typeString` =>
            c.downField("signatures")
              .as[ListMap[PublicKeyPropositionEd25519, Proof[PublicKeyPropositionEd25519]]]
              .map(AssetTransfer(from, to, _, fee, timestamp, data, minting))
        }
      } yield transfer

  implicit def polyTransferJsonEncoder[P <: Proposition]: Encoder[PolyTransfer[P]] = { tx: PolyTransfer[P] =>
    Map(
      "txId"            -> tx.id.asJson,
      "txType"          -> "PolyTransfer".asJson,
      "propositionType" -> tx.getPropIdentifier.typeString.asJson,
      "newBoxes"        -> tx.newBoxes.toSeq.asJson,
      "boxesToRemove"   -> tx.boxIdsToOpen.asJson,
      "from"            -> encodeFrom(tx.from),
      "to"              -> tx.to.asJson,
      "signatures"      -> tx.attestation.asJson,
      "fee"             -> tx.fee.asJson,
      "timestamp"       -> tx.timestamp.asJson,
      "minting"         -> tx.minting.asJson,
      "data"            -> tx.data.asJson
    ).asJson
  }

  implicit def polyTransferJsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[PolyTransfer[_ <: Proposition]] =
    (c: HCursor) =>
      for {
        from      <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to        <- c.downField("to").as[IndexedSeq[(Address, SimpleValue)]]
        fee       <- c.get[Int128]("fee")
        timestamp <- c.downField("timestamp").as[Long]
        data      <- c.downField("data").as[Option[Latin1Data]]
        propType  <- c.downField("propositionType").as[String]
        minting   <- c.downField("minting").as[Boolean]
        transfer <- propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            c.downField("signatures")
              .as[ListMap[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]]
              .map(PolyTransfer(from, to, _, fee, timestamp, data, minting))
          case ThresholdPropositionCurve25519.`typeString` =>
            c.downField("signatures")
              .as[ListMap[ThresholdPropositionCurve25519, Proof[ThresholdPropositionCurve25519]]]
              .map(PolyTransfer(from, to, _, fee, timestamp, data, minting))
          case PublicKeyPropositionEd25519.`typeString` =>
            c.downField("signatures")
              .as[ListMap[PublicKeyPropositionEd25519, Proof[PublicKeyPropositionEd25519]]]
              .map(PolyTransfer(from, to, _, fee, timestamp, data, minting))
        }
      } yield transfer

  implicit def transactionJsonTypedEncoder[T, P <: Proposition]: Encoder[Transaction[T, P]] = { tx =>
    txJsonEncoder(tx)
  }

  implicit def txJsonEncoder: Encoder[TX] = {
    //    case tx: CodeCreation           => CodeCreation.jsonEncoder(tx)
    //    case tx: ProgramCreation        => ProgramCreation.jsonEncoder(tx)
    //    case tx: ProgramMethodExecution => ProgramMethodExecution.jsonEncoder(tx)
    //    case tx: ProgramTransfer        => ProgramTransfer.jsonEncoder(tx)
    case tx: PolyTransfer[_]  => polyTransferJsonEncoder(tx)
    case tx: ArbitTransfer[_] => arbitTransferJsonEncoder(tx)
    case tx: AssetTransfer[_] => assetTransferJsonEncoder(tx)
  }

  implicit def txJsonDecoder(implicit networkPrefix: NetworkPrefix): Decoder[TX] = { c: HCursor =>
    c.downField("txType")
      .as[String]
      .map {
        //      case "CodeCreation"           => CodeCreation.jsonDecoder(c)
        //      case "ProgramCreation"        => ProgramCreation.jsonDecoder(c)
        //      case "ProgramMethodExecution" => ProgramMethodExecution.jsonDecoder(c)
        //      case "ProgramTransfer"        => ProgramTransfer.jsonDecoder(c)
        case PolyTransfer.typeString  => polyTransferJsonDecoder(networkPrefix)(c)
        case ArbitTransfer.typeString => arbitTransferJsonDecoder(networkPrefix)(c)
        case AssetTransfer.typeString => assetTransferJsonDecoder(networkPrefix)(c)
      } match {
      case Right(tx) => tx
      case Left(ex)  => throw ex
    }
  }

}
