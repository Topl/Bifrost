package co.topl.modifier.transaction.builder

import co.topl.attestation._
import co.topl.modifier.box.{AssetValue, SimpleValue}
import co.topl.modifier.transaction.ArbitTransfer.Validation.InvalidArbitTransfer
import co.topl.modifier.transaction.AssetTransfer.Validation.InvalidAssetTransfer
import co.topl.modifier.transaction.PolyTransfer.Validation.InvalidPolyTransfer
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.{Identifiable, Int128}

object TransferBlueprints {

  case class PolyTransferBlueprint(recipients: IndexedSeq[(Address, SimpleValue)])

  case class AssetTransferBlueprint(
    consolidationAddress: Address,
    recipients:           IndexedSeq[(Address, AssetValue)],
    minting:              Boolean
  )

  case class ArbitTransferBlueprint(consolidationAddress: Address, recipients: IndexedSeq[(Address, SimpleValue)])

  trait Instances {

    private def polyTransferBlueprint[P <: Proposition: EvidenceProducer: Identifiable]
      : TransferBlueprint[PolyTransferBlueprint, InvalidPolyTransfer, PolyTransfer[P]] =
      (
        blueprint:      PolyTransferBlueprint,
        availableBoxes: TokenBoxes,
        changeAddress:  Address,
        fee:            Int128,
        data:           Option[Latin1Data]
      ) => PolyTransfer.validated[P](availableBoxes.polys, blueprint.recipients, changeAddress, fee, data)

    private def arbitTransferBlueprint[P <: Proposition: EvidenceProducer: Identifiable]
      : TransferBlueprint[ArbitTransferBlueprint, InvalidArbitTransfer, ArbitTransfer[P]] =
      (
        blueprint:      ArbitTransferBlueprint,
        availableBoxes: TokenBoxes,
        changeAddress:  Address,
        fee:            Int128,
        data:           Option[Latin1Data]
      ) =>
        ArbitTransfer.validated[P](
          availableBoxes.polys,
          availableBoxes.arbits,
          blueprint.recipients,
          changeAddress,
          blueprint.consolidationAddress,
          fee,
          data
        )

    private def assetTransferBlueprint[P <: Proposition: EvidenceProducer: Identifiable]
      : TransferBlueprint[AssetTransferBlueprint, InvalidAssetTransfer, AssetTransfer[P]] =
      (
        blueprint:      AssetTransferBlueprint,
        availableBoxes: TokenBoxes,
        changeAddress:  Address,
        fee:            Int128,
        data:           Option[Latin1Data]
      ) =>
        AssetTransfer.validated[P](
          availableBoxes.polys,
          availableBoxes.assets,
          blueprint.recipients,
          changeAddress,
          blueprint.consolidationAddress,
          fee,
          data,
          blueprint.minting
        )

    implicit val polyTransferPublicKeyCurveBlueprint
      : TransferBlueprint[PolyTransferBlueprint, InvalidPolyTransfer, PolyTransfer[PublicKeyPropositionCurve25519]] =
      polyTransferBlueprint[PublicKeyPropositionCurve25519]

    implicit val polyTransferPublicKeyEdBlueprint
      : TransferBlueprint[PolyTransferBlueprint, InvalidPolyTransfer, PolyTransfer[PublicKeyPropositionEd25519]] =
      polyTransferBlueprint[PublicKeyPropositionEd25519]

    implicit val polyTransferThresholdCurveBlueprint
      : TransferBlueprint[PolyTransferBlueprint, InvalidPolyTransfer, PolyTransfer[ThresholdPropositionCurve25519]] =
      polyTransferBlueprint[ThresholdPropositionCurve25519]

    implicit val arbitTransferPublicKeyCurveBlueprint
      : TransferBlueprint[ArbitTransferBlueprint, InvalidArbitTransfer, ArbitTransfer[PublicKeyPropositionCurve25519]] =
      arbitTransferBlueprint[PublicKeyPropositionCurve25519]

    implicit val arbitTransferPublicKeyEdBlueprint
      : TransferBlueprint[ArbitTransferBlueprint, InvalidArbitTransfer, ArbitTransfer[PublicKeyPropositionEd25519]] =
      arbitTransferBlueprint[PublicKeyPropositionEd25519]

    implicit val arbitTransferThresholdCurveBlueprint
      : TransferBlueprint[ArbitTransferBlueprint, InvalidArbitTransfer, ArbitTransfer[ThresholdPropositionCurve25519]] =
      arbitTransferBlueprint[ThresholdPropositionCurve25519]

    implicit val assetTransferPublicKeyCurveBlueprint
      : TransferBlueprint[AssetTransferBlueprint, InvalidAssetTransfer, AssetTransfer[PublicKeyPropositionCurve25519]] =
      assetTransferBlueprint[PublicKeyPropositionCurve25519]

    implicit val assetTransferPublicKeyEdBlueprint
      : TransferBlueprint[AssetTransferBlueprint, InvalidAssetTransfer, AssetTransfer[PublicKeyPropositionEd25519]] =
      assetTransferBlueprint[PublicKeyPropositionEd25519]

    implicit val assetTransferThresholdCurveBlueprint
      : TransferBlueprint[AssetTransferBlueprint, InvalidAssetTransfer, AssetTransfer[ThresholdPropositionCurve25519]] =
      assetTransferBlueprint[ThresholdPropositionCurve25519]
  }

  trait Implicits extends Instances

  object implicits extends Implicits
}
