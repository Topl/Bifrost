package co.topl.modifier.transaction.builder

import co.topl.attestation._
import co.topl.modifier.box.{AssetValue, SimpleValue}
import co.topl.modifier.transaction.ArbitTransfer.Validation.InvalidArbitTransfer
import co.topl.modifier.transaction.AssetTransfer.Validation.InvalidAssetTransfer
import co.topl.modifier.transaction.PolyTransfer.Validation.InvalidPolyTransfer
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.{Identifiable, Int128}

import scala.language.implicitConversions

trait ValueTransferBlueprint[Value, Failure, Transfer] {

  def compileBlueprint(
    availableBoxes:       TokenBoxes,
    recipients:           IndexedSeq[(Address, Value)],
    feeChangeAddress:     Address,
    consolidationAddress: Address,
    fee:                  Int128,
    data:                 Option[Latin1Data],
    minting:              Boolean
  ): Either[Failure, Transfer]
}

object ValueTransferBlueprint {

  private def polyTransferBlueprint[P <: Proposition: EvidenceProducer: Identifiable]
    : ValueTransferBlueprint[SimpleValue, InvalidPolyTransfer, PolyTransfer[P]] =
    (
      availableBoxes: TokenBoxes,
      recipients:     IndexedSeq[(Address, SimpleValue)],
      changeAddress:  Address,
      _:              Address, // ignore consolidation address
      fee:            Int128,
      data:           Option[Latin1Data],
      minting:        Boolean
    ) => PolyTransfer.validated[P](availableBoxes.polys, recipients, changeAddress, fee, data, minting)

  private def arbitTransferBlueprint[P <: Proposition: EvidenceProducer: Identifiable]
    : ValueTransferBlueprint[SimpleValue, InvalidArbitTransfer, ArbitTransfer[P]] =
    (
      availableBoxes:       TokenBoxes,
      recipients:           IndexedSeq[(Address, SimpleValue)],
      changeAddress:        Address,
      consolidationAddress: Address,
      fee:                  Int128,
      data:                 Option[Latin1Data],
      minting:              Boolean
    ) =>
      ArbitTransfer.validated[P](
        availableBoxes.polys,
        availableBoxes.arbits,
        recipients,
        changeAddress,
        consolidationAddress,
        fee,
        data,
        minting
      )

  private def assetTransferBlueprint[P <: Proposition: EvidenceProducer: Identifiable]
    : ValueTransferBlueprint[AssetValue, InvalidAssetTransfer, AssetTransfer[P]] =
    (
      availableBoxes:       TokenBoxes,
      recipients:           IndexedSeq[(Address, AssetValue)],
      changeAddress:        Address,
      consolidationAddress: Address,
      fee:                  Int128,
      data:                 Option[Latin1Data],
      minting:              Boolean
    ) =>
      AssetTransfer.validated[P](
        availableBoxes.polys,
        availableBoxes.assets,
        recipients,
        changeAddress,
        consolidationAddress,
        fee,
        data,
        minting
      )

  trait Instances {

    implicit val polyTransferPublicKeyCurveBlueprint
      : ValueTransferBlueprint[SimpleValue, InvalidPolyTransfer, PolyTransfer[
        PublicKeyPropositionCurve25519
      ]] =
      polyTransferBlueprint[PublicKeyPropositionCurve25519]

    implicit val polyTransferPublicKeyEdBlueprint
      : ValueTransferBlueprint[SimpleValue, InvalidPolyTransfer, PolyTransfer[PublicKeyPropositionEd25519]] =
      polyTransferBlueprint[PublicKeyPropositionEd25519]

    implicit val polyTransferThresholdCurveBlueprint
      : ValueTransferBlueprint[SimpleValue, InvalidPolyTransfer, PolyTransfer[
        ThresholdPropositionCurve25519
      ]] =
      polyTransferBlueprint[ThresholdPropositionCurve25519]

    implicit val arbitTransferPublicKeyCurveBlueprint
      : ValueTransferBlueprint[SimpleValue, InvalidArbitTransfer, ArbitTransfer[
        PublicKeyPropositionCurve25519
      ]] =
      arbitTransferBlueprint[PublicKeyPropositionCurve25519]

    implicit val arbitTransferPublicKeyEdBlueprint
      : ValueTransferBlueprint[SimpleValue, InvalidArbitTransfer, ArbitTransfer[
        PublicKeyPropositionEd25519
      ]] =
      arbitTransferBlueprint[PublicKeyPropositionEd25519]

    implicit val arbitTransferThresholdCurveBlueprint
      : ValueTransferBlueprint[SimpleValue, InvalidArbitTransfer, ArbitTransfer[
        ThresholdPropositionCurve25519
      ]] =
      arbitTransferBlueprint[ThresholdPropositionCurve25519]

    implicit val assetTransferPublicKeyCurveBlueprint
      : ValueTransferBlueprint[AssetValue, InvalidAssetTransfer, AssetTransfer[
        PublicKeyPropositionCurve25519
      ]] =
      assetTransferBlueprint[PublicKeyPropositionCurve25519]

    implicit val assetTransferPublicKeyEdBlueprint
      : ValueTransferBlueprint[AssetValue, InvalidAssetTransfer, AssetTransfer[
        PublicKeyPropositionEd25519
      ]] =
      assetTransferBlueprint[PublicKeyPropositionEd25519]

    implicit val assetTransferThresholdCurveBlueprint
      : ValueTransferBlueprint[AssetValue, InvalidAssetTransfer, AssetTransfer[
        ThresholdPropositionCurve25519
      ]] =
      assetTransferBlueprint[ThresholdPropositionCurve25519]
  }

  trait Implicits extends Instances

  object implicits extends Implicits
}
