package co.topl.codecs.binary.scodecs.modifier.transaction

import co.topl.attestation._
import co.topl.codecs.binary.scodecs.attestation._
import co.topl.codecs.binary.scodecs.modifier.box._
import co.topl.codecs.binary.scodecs.valuetypes._
import co.topl.modifier.box.Box.Nonce
import co.topl.modifier.box.{SimpleValue, TokenValueHolder}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.utils.Identifiable
import scodec.codecs.{byte, discriminated}
import scodec.{Attempt, Codec}
import shapeless.{::, HList, HNil}

import scala.collection.immutable.ListMap

trait TransactionCodecs {

  implicit def polyTransferWithPropositionCodec[P <: Proposition: Identifiable: EvidenceProducer]
    : Codec[PolyTransfer[P]] =
    (indexedSeqCodec(tupleCodec(addressCodec, longCodec)) ::
      indexedSeqCodec(
        tupleCodec(
          addressCodec,
          tokenValueHolderCodec.xmap[SimpleValue](token => token.asInstanceOf[SimpleValue], value => value)
        )
      ) ::
      listMapCodec(propositionCodec, proofCodec) ::
      int128Codec ::
      uLongCodec ::
      optionCodec(latin1DataCodec) ::
      boolCodec)
      .xmapc { case from :: to :: attestation :: fee :: timestamp :: data :: minting :: HNil =>
        PolyTransfer[P](
          from,
          to,
          attestation.asInstanceOf[ListMap[P, Proof[P]]],
          fee,
          timestamp,
          data,
          minting
        )
      } { polyTransfer =>
        HList(
          polyTransfer.from,
          polyTransfer.to,
          polyTransfer.attestation.asInstanceOf[ListMap[Proposition, Proof[P]]],
          polyTransfer.fee,
          polyTransfer.timestamp,
          polyTransfer.data,
          polyTransfer.minting
        )
      }
      .as[PolyTransfer[P]]

  implicit def arbitTransferWithPropositionCodec[P <: Proposition: Identifiable: EvidenceProducer]
    : Codec[ArbitTransfer[P]] =
    (indexedSeqCodec(tupleCodec(addressCodec, longCodec)) ::
      indexedSeqCodec(
        tupleCodec(
          addressCodec,
          tokenValueHolderCodec.xmap[SimpleValue](token => token.asInstanceOf[SimpleValue], value => value)
        )
      ) ::
      listMapCodec(propositionCodec, proofCodec) ::
      int128Codec ::
      uLongCodec ::
      optionCodec(latin1DataCodec) ::
      boolCodec)
      .xmapc { case from :: to :: attestation :: fee :: timestamp :: data :: minting :: HNil =>
        ArbitTransfer[P](from, to, attestation.asInstanceOf[ListMap[P, Proof[P]]], fee, timestamp, data, minting)
      } { arbitTransfer =>
        HList(
          arbitTransfer.from,
          arbitTransfer.to,
          arbitTransfer.attestation.asInstanceOf[ListMap[Proposition, Proof[P]]],
          arbitTransfer.fee,
          arbitTransfer.timestamp,
          arbitTransfer.data,
          arbitTransfer.minting
        )
      }
      .as[ArbitTransfer[P]]

  implicit def assetTransferWithPropositionCodec[P <: Proposition: Identifiable: EvidenceProducer]
    : Codec[AssetTransfer[P]] =
    (indexedSeqCodec(tupleCodec(addressCodec, longCodec)) ::
      indexedSeqCodec(tupleCodec(addressCodec, tokenValueHolderCodec)) ::
      listMapCodec(propositionCodec, proofCodec) ::
      int128Codec ::
      uLongCodec ::
      optionCodec(latin1DataCodec) ::
      boolCodec)
      .xmapc { case from :: to :: attestation :: fee :: timestamp :: data :: minting :: HNil =>
        AssetTransfer[P](from, to, attestation.asInstanceOf[ListMap[P, Proof[P]]], fee, timestamp, data, minting)
      } { assetTransfer =>
        HList(
          assetTransfer.from,
          assetTransfer.to,
          assetTransfer.attestation.asInstanceOf[ListMap[Proposition, Proof[P]]],
          assetTransfer.fee,
          assetTransfer.timestamp,
          assetTransfer.data,
          assetTransfer.minting
        )
      }
      .as[AssetTransfer[P]]

  // NOTE: for the following codecs, the underlying types of propositions for each transfer type
  // are unknown at runtime and must be ignored using @unchecked to avoid warnings.
  // If the type of the proposition does not match the expected type for some reason, the codec will fail.

  implicit val polyTransferCodec: Codec[PolyTransfer[_ <: Proposition]] =
    byteCodec.consume[PolyTransfer[_ <: Proposition]] {
      case PublicKeyPropositionCurve25519.typePrefix =>
        polyTransferWithPropositionCodec[PublicKeyPropositionCurve25519]
          .exmapc[PolyTransfer[_ <: Proposition]](transfer => Attempt.successful(transfer)) {
            case x: PolyTransfer[PublicKeyPropositionCurve25519] @unchecked =>
              Attempt.successful(x)
          }
      case ThresholdPropositionCurve25519.typePrefix =>
        polyTransferWithPropositionCodec[ThresholdPropositionCurve25519]
          .exmapc[PolyTransfer[_ <: Proposition]](transfer => Attempt.successful(transfer)) {
            case x: PolyTransfer[ThresholdPropositionCurve25519] @unchecked =>
              Attempt.successful(x)
          }
      case PublicKeyPropositionEd25519.typePrefix =>
        polyTransferWithPropositionCodec[PublicKeyPropositionEd25519]
          .exmapc[PolyTransfer[_ <: Proposition]](transfer => Attempt.successful(transfer)) {
            case x: PolyTransfer[PublicKeyPropositionEd25519] @unchecked =>
              Attempt.successful(x)
          }
    }(transfer => transfer.getPropIdentifier.typePrefix)

  implicit val arbitTransferCodec: Codec[ArbitTransfer[_ <: Proposition]] =
    byteCodec.consume[ArbitTransfer[_ <: Proposition]] {
      case PublicKeyPropositionCurve25519.typePrefix =>
        arbitTransferWithPropositionCodec[PublicKeyPropositionCurve25519]
          .exmapc[ArbitTransfer[_ <: Proposition]](transfer => Attempt.successful(transfer)) {
            case x: ArbitTransfer[PublicKeyPropositionCurve25519] @unchecked =>
              Attempt.successful(x)
          }
      case ThresholdPropositionCurve25519.typePrefix =>
        arbitTransferWithPropositionCodec[ThresholdPropositionCurve25519]
          .exmapc[ArbitTransfer[_ <: Proposition]](transfer => Attempt.successful(transfer)) {
            case x: ArbitTransfer[ThresholdPropositionCurve25519] @unchecked =>
              Attempt.successful(x)
          }
      case PublicKeyPropositionEd25519.typePrefix =>
        arbitTransferWithPropositionCodec[PublicKeyPropositionEd25519]
          .exmapc[ArbitTransfer[_ <: Proposition]](transfer => Attempt.successful(transfer)) {
            case x: ArbitTransfer[PublicKeyPropositionEd25519] @unchecked =>
              Attempt.successful(x)
          }
    }(transfer => transfer.getPropIdentifier.typePrefix)

  implicit val assetTransferCodec: Codec[AssetTransfer[_ <: Proposition]] =
    byteCodec.consume[AssetTransfer[_ <: Proposition]] {
      case PublicKeyPropositionCurve25519.typePrefix =>
        assetTransferWithPropositionCodec[PublicKeyPropositionCurve25519]
          .exmapc[AssetTransfer[_ <: Proposition]](transfer => Attempt.successful(transfer)) {
            case x: AssetTransfer[PublicKeyPropositionCurve25519] @unchecked =>
              Attempt.successful(x)
          }
      case ThresholdPropositionCurve25519.typePrefix =>
        assetTransferWithPropositionCodec[ThresholdPropositionCurve25519]
          .exmapc[AssetTransfer[_ <: Proposition]](transfer => Attempt.successful(transfer)) {
            case x: AssetTransfer[ThresholdPropositionCurve25519] @unchecked =>
              Attempt.successful(x)
          }
      case PublicKeyPropositionEd25519.typePrefix =>
        assetTransferWithPropositionCodec[PublicKeyPropositionEd25519]
          .exmapc[AssetTransfer[_ <: Proposition]](transfer => Attempt.successful(transfer)) {
            case x: AssetTransfer[PublicKeyPropositionEd25519] @unchecked =>
              Attempt.successful(x)
          }
    }(transfer => transfer.getPropIdentifier.typePrefix)

  implicit val transactionCodec: Codec[Transaction.TX] =
    discriminated[Transaction.TX]
      .by(byte)
      .typecase(PolyTransfer.typePrefix, polyTransferCodec)
      .typecase(ArbitTransfer.typePrefix, arbitTransferCodec)
      .typecase(AssetTransfer.typePrefix, assetTransferCodec)
}
