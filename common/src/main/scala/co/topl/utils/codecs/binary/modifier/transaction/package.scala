package co.topl.utils.codecs.binary.modifier

import co.topl.attestation._
import co.topl.modifier.box.Box.Nonce
import co.topl.modifier.box.{SimpleValue, TokenValueHolder}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.utils.Identifiable
import co.topl.utils.codecs.binary.attestation.codecs._
import co.topl.utils.codecs.binary.modifier.box.codecs._
import co.topl.utils.codecs.binary.valuetypes.implicits._
import scodec.Codec
import scodec.codecs.{byte, discriminated}
import shapeless._

package object transaction {

  trait Codecs {

    implicit def polyTransferWithPropositionCodec[P <: Proposition: Identifiable: EvidenceProducer](implicit
      propCodec:  Codec[P],
      proofCodec: Codec[Proof[P]]
    ): Codec[PolyTransfer[P]] =
      (listCodec(tupleCodec(addressCodec, longCodec)).as[IndexedSeq[(Address, Nonce)]] ::
        listCodec(tupleCodec(addressCodec, simpleValueCodec)).as[IndexedSeq[(Address, SimpleValue)]] ::
        listMapCodec(propCodec, proofCodec) ::
        int128Codec ::
        longCodec ::
        optionCodec(latin1DataCodec) ::
        boolCodec)
        .xmapc { case from :: to :: attestation :: fee :: timestamp :: data :: minting :: HNil =>
          PolyTransfer[P](from, to, attestation, fee, timestamp, data, minting)
        } { polyTransfer =>
          HList(
            polyTransfer.from,
            polyTransfer.to,
            polyTransfer.attestation,
            polyTransfer.fee,
            polyTransfer.timestamp,
            polyTransfer.data,
            polyTransfer.minting
          )
        }
        .as[PolyTransfer[P]]

    implicit def arbitTransferWithPropositionCodec[P <: Proposition: Identifiable: EvidenceProducer](implicit
      propCodec:  Codec[P],
      proofCodec: Codec[Proof[P]]
    ): Codec[ArbitTransfer[P]] =
      (listCodec(tupleCodec(addressCodec, longCodec)).as[IndexedSeq[(Address, Nonce)]] ::
        listCodec(tupleCodec(addressCodec, simpleValueCodec)).as[IndexedSeq[(Address, SimpleValue)]] ::
        listMapCodec(propCodec, proofCodec) ::
        int128Codec ::
        longCodec ::
        optionCodec(latin1DataCodec) ::
        boolCodec)
        .xmapc { case from :: to :: attestation :: fee :: timestamp :: data :: minting :: HNil =>
          ArbitTransfer[P](from, to, attestation, fee, timestamp, data, minting)
        } { arbitTransfer =>
          HList(
            arbitTransfer.from,
            arbitTransfer.to,
            arbitTransfer.attestation,
            arbitTransfer.fee,
            arbitTransfer.timestamp,
            arbitTransfer.data,
            arbitTransfer.minting
          )
        }
        .as[ArbitTransfer[P]]

    implicit def assetTransferWithPropositionCodec[P <: Proposition: Identifiable: EvidenceProducer](implicit
      propCodec:  Codec[P],
      proofCodec: Codec[Proof[P]]
    ): Codec[AssetTransfer[P]] =
      (listCodec(tupleCodec(addressCodec, longCodec)).as[IndexedSeq[(Address, Nonce)]] ::
        listCodec(tupleCodec(addressCodec, tokenValueHolderCodec)).as[IndexedSeq[(Address, TokenValueHolder)]] ::
        listMapCodec(propCodec, proofCodec) ::
        int128Codec ::
        longCodec ::
        optionCodec(latin1DataCodec) ::
        boolCodec)
        .xmapc { case from :: to :: attestation :: fee :: timestamp :: data :: minting :: HNil =>
          AssetTransfer[P](from, to, attestation, fee, timestamp, data, minting)
        } { assetTransfer =>
          HList(
            assetTransfer.from,
            assetTransfer.to,
            assetTransfer.attestation,
            assetTransfer.fee,
            assetTransfer.timestamp,
            assetTransfer.data,
            assetTransfer.minting
          )
        }
        .as[AssetTransfer[P]]

    implicit val polyTransferCodec: Codec[PolyTransfer[_ <: Proposition]] =
      discriminated[PolyTransfer[_ <: Proposition]]
        .by(byteCodec)
        .typecase(
          PublicKeyPropositionCurve25519.typePrefix,
          polyTransferWithPropositionCodec[PublicKeyPropositionCurve25519]
        )
        .typecase(PublicKeyPropositionEd25519.typePrefix, polyTransferWithPropositionCodec[PublicKeyPropositionEd25519])
        .typecase(
          ThresholdPropositionCurve25519.typePrefix,
          polyTransferWithPropositionCodec[ThresholdPropositionCurve25519]
        )

    implicit val arbitTransferCodec: Codec[ArbitTransfer[_ <: Proposition]] =
      discriminated[ArbitTransfer[_ <: Proposition]]
        .by(byteCodec)
        .typecase(
          PublicKeyPropositionCurve25519.typePrefix,
          arbitTransferWithPropositionCodec[PublicKeyPropositionCurve25519]
        )
        .typecase(
          PublicKeyPropositionEd25519.typePrefix,
          arbitTransferWithPropositionCodec[PublicKeyPropositionEd25519]
        )
        .typecase(
          ThresholdPropositionCurve25519.typePrefix,
          arbitTransferWithPropositionCodec[ThresholdPropositionCurve25519]
        )

    implicit val assetTransferCodec: Codec[AssetTransfer[_ <: Proposition]] =
      discriminated[AssetTransfer[_ <: Proposition]]
        .by(byteCodec)
        .typecase(
          PublicKeyPropositionCurve25519.typePrefix,
          assetTransferWithPropositionCodec[PublicKeyPropositionCurve25519]
        )
        .typecase(
          PublicKeyPropositionEd25519.typePrefix,
          assetTransferWithPropositionCodec[PublicKeyPropositionEd25519]
        )
        .typecase(
          ThresholdPropositionCurve25519.typePrefix,
          assetTransferWithPropositionCodec[ThresholdPropositionCurve25519]
        )

    implicit val transactionCodec: Codec[Transaction.TX] =
      discriminated[Transaction.TX]
        .by(byte)
        .typecase(PolyTransfer.typePrefix, polyTransferCodec)
        .typecase(ArbitTransfer.typePrefix, arbitTransferCodec)
        .typecase(AssetTransfer.typePrefix, assetTransferCodec)
  }

  object codecs extends Codecs

}
