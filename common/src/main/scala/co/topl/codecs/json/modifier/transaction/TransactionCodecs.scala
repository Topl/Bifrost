package co.topl.codecs.json.modifier.transaction

import cats.implicits._
import co.topl.attestation._
import co.topl.codecs.json.attestation._
import co.topl.codecs.json.modifier.block._
import co.topl.codecs.json.modifier.box._
import co.topl.codecs.json.valuetypes._
import co.topl.modifier.box.{Box, SimpleValue, TokenValueHolder}
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction.TransferTransaction.encodeFrom
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}
import co.topl.codecs.binary._
import co.topl.modifier.transaction
import io.circe

import scala.collection.immutable.ListMap

trait TransactionCodecs {

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
        from           <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to             <- c.downField("to").as[IndexedSeq[(Address, SimpleValue)]]
        fee            <- c.get[Int128]("fee")
        timestamp      <- c.downField("timestamp").as[Long]
        data           <- c.downField("data").as[Option[Latin1Data]]
        propType       <- c.downField("propositionType").as[String]
        minting        <- c.downField("minting").as[Boolean]
        signaturesData <- c.downField("signatures").as[ListMap[Base58Data, Base58Data]]
        transfer <- propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            signaturesData
              .foldLeft(
                ListMap[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]()
                  .asRight[DecodingFailure]
              ) {
                case (Right(signatures), (key, value)) =>
                  (for {
                    publicKey <- key.encodeAsBytes.tail.decodeTransmitted[PublicKeyPropositionCurve25519]
                    sig       <- value.encodeAsBytes.tail.decodeTransmitted[SignatureCurve25519]
                  } yield signatures ++ ListMap(publicKey -> sig))
                    .leftMap(err => DecodingFailure(err, List()))
                case (err, _) => err
              }
              .map(ArbitTransfer(from, to, _, fee, timestamp, data, minting))
          case ThresholdPropositionCurve25519.`typeString` =>
            signaturesData
              .foldLeft(
                ListMap[ThresholdPropositionCurve25519, Proof[ThresholdPropositionCurve25519]]()
                  .asRight[DecodingFailure]
              ) {
                case (Right(signatures), (key, value)) =>
                  (for {
                    publicKey <- key.encodeAsBytes.tail.decodeTransmitted[ThresholdPropositionCurve25519]
                    sig       <- value.encodeAsBytes.tail.decodeTransmitted[ThresholdSignatureCurve25519]
                  } yield signatures ++ ListMap(publicKey -> sig))
                    .leftMap(err => DecodingFailure(err, List()))
                case (err, _) => err
              }
              .map(ArbitTransfer(from, to, _, fee, timestamp, data, minting))
          case PublicKeyPropositionEd25519.`typeString` =>
            signaturesData
              .foldLeft(
                ListMap[PublicKeyPropositionEd25519, Proof[PublicKeyPropositionEd25519]]().asRight[DecodingFailure]
              ) {
                case (Right(signatures), (key, value)) =>
                  (for {
                    publicKey <- key.encodeAsBytes.tail.decodeTransmitted[PublicKeyPropositionEd25519]
                    sig       <- value.encodeAsBytes.tail.decodeTransmitted[SignatureEd25519]
                  } yield signatures ++ ListMap(publicKey -> sig))
                    .leftMap(err => DecodingFailure(err, List()))
                case (err, _) => err
              }
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
        from           <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to             <- c.downField("to").as[IndexedSeq[(Address, TokenValueHolder)]]
        fee            <- c.get[Int128]("fee")
        timestamp      <- c.downField("timestamp").as[Long]
        data           <- c.downField("data").as[Option[Latin1Data]]
        minting        <- c.downField("minting").as[Boolean]
        propType       <- c.downField("propositionType").as[String]
        signaturesData <- c.downField("signatures").as[ListMap[Base58Data, Base58Data]]
        transfer <- propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            signaturesData
              .foldLeft(
                ListMap[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]()
                  .asRight[DecodingFailure]
              ) {
                case (Right(signatures), (key, value)) =>
                  (for {
                    publicKey <- key.encodeAsBytes.tail.decodeTransmitted[PublicKeyPropositionCurve25519]
                    sig       <- value.encodeAsBytes.tail.decodeTransmitted[SignatureCurve25519]
                  } yield signatures ++ ListMap(publicKey -> sig))
                    .leftMap(err => DecodingFailure(err, List()))
                case (err, _) => err
              }
              .map(AssetTransfer(from, to, _, fee, timestamp, data, minting))
          case ThresholdPropositionCurve25519.`typeString` =>
            signaturesData
              .foldLeft(
                ListMap[ThresholdPropositionCurve25519, Proof[ThresholdPropositionCurve25519]]()
                  .asRight[DecodingFailure]
              ) {
                case (Right(signatures), (key, value)) =>
                  (for {
                    publicKey <- key.encodeAsBytes.tail.decodeTransmitted[ThresholdPropositionCurve25519]
                    sig       <- value.encodeAsBytes.tail.decodeTransmitted[ThresholdSignatureCurve25519]
                  } yield signatures ++ ListMap(publicKey -> sig))
                    .leftMap(err => DecodingFailure(err, List()))
                case (err, _) => err
              }
              .map(AssetTransfer(from, to, _, fee, timestamp, data, minting))
          case PublicKeyPropositionEd25519.`typeString` =>
            signaturesData
              .foldLeft(
                ListMap[PublicKeyPropositionEd25519, Proof[PublicKeyPropositionEd25519]]().asRight[DecodingFailure]
              ) {
                case (Right(signatures), (key, value)) =>
                  (for {
                    publicKey <- key.encodeAsBytes.tail.decodeTransmitted[PublicKeyPropositionEd25519]
                    sig       <- value.encodeAsBytes.tail.decodeTransmitted[SignatureEd25519]
                  } yield signatures ++ ListMap(publicKey -> sig))
                    .leftMap(err => DecodingFailure(err, List()))
                case (err, _) => err
              }
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
        from           <- c.downField("from").as[IndexedSeq[(Address, Box.Nonce)]]
        to             <- c.downField("to").as[IndexedSeq[(Address, SimpleValue)]]
        fee            <- c.get[Int128]("fee")
        timestamp      <- c.downField("timestamp").as[Long]
        data           <- c.downField("data").as[Option[Latin1Data]]
        propType       <- c.downField("propositionType").as[String]
        minting        <- c.downField("minting").as[Boolean]
        signaturesData <- c.downField("signatures").as[ListMap[Base58Data, Base58Data]]
        transfer <- propType match {
          case PublicKeyPropositionCurve25519.`typeString` =>
            signaturesData
              .foldLeft(
                ListMap[PublicKeyPropositionCurve25519, Proof[PublicKeyPropositionCurve25519]]()
                  .asRight[DecodingFailure]
              ) {
                case (Right(signatures), (key, value)) =>
                  (for {
                    publicKey <- key.encodeAsBytes.tail.decodeTransmitted[PublicKeyPropositionCurve25519]
                    sig       <- value.encodeAsBytes.tail.decodeTransmitted[SignatureCurve25519]
                  } yield signatures ++ ListMap(publicKey -> sig))
                    .leftMap(err => DecodingFailure(err, List()))
                case (err, _) => err
              }
              .map(PolyTransfer(from, to, _, fee, timestamp, data, minting))
          case ThresholdPropositionCurve25519.`typeString` =>
            signaturesData
              .foldLeft(
                ListMap[ThresholdPropositionCurve25519, Proof[ThresholdPropositionCurve25519]]()
                  .asRight[DecodingFailure]
              ) {
                case (Right(signatures), (key, value)) =>
                  (for {
                    publicKey <- key.encodeAsBytes.tail.decodeTransmitted[ThresholdPropositionCurve25519]
                    sig       <- value.encodeAsBytes.tail.decodeTransmitted[ThresholdSignatureCurve25519]
                  } yield signatures ++ ListMap(publicKey -> sig))
                    .leftMap(err => DecodingFailure(err, List()))
                case (err, _) => err
              }
              .map(PolyTransfer(from, to, _, fee, timestamp, data, minting))
          case PublicKeyPropositionEd25519.`typeString` =>
            signaturesData
              .foldLeft(
                ListMap[PublicKeyPropositionEd25519, Proof[PublicKeyPropositionEd25519]]().asRight[DecodingFailure]
              ) {
                case (Right(signatures), (key, value)) =>
                  (for {
                    publicKey <- key.encodeAsBytes.tail.decodeTransmitted[PublicKeyPropositionEd25519]
                    sig       <- value.encodeAsBytes.tail.decodeTransmitted[SignatureEd25519]
                  } yield signatures ++ ListMap(publicKey -> sig))
                    .leftMap(err => DecodingFailure(err, List()))
                case (err, _) => err
              }
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
