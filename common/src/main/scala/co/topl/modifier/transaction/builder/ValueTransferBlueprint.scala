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

/**
 * Defines a blueprint for creating a transfer transaction from a set of input parameters.
 * @tparam Value the value of the token to send
 * @tparam Failure the type of validation failure that can occur
 * @tparam Transfer the type of transfer that is generated
 */
trait ValueTransferBlueprint[Value, Failure, Transfer] {

  /**
   * Compiles a set of parameters into a value transfer transaction using the given blueprint.
   * @param availableBoxes the available boxes that can be used as inputs
   * @param recipients the addresses that will receive token boxes as part of the transfer
   * @param feeChangeAddress the address that should receive any change after paying the transaction fee
   * @param consolidationAddress the address that should recieve any change after paying tokens to the recipients
   * @param fee the fee amount to pay
   * @param data the metadata text to add to the transaction
   * @param minting whether or not the output token boxes should be minted
   * @return either a transfer transaction of type `Transfer` or a validation failure of type `Failure`
   */
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

  /**
   * Defines a `ValueTransferBlueprint` type-class instance for `PolyTransfer` for a given proposition.
   * @tparam P the type of proposition on the poly transfer
   * @return a type-class instance of `ValueTransferBlueprint` for `PolyTransfer`
   */
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

  /**
   * Defines a `ValueTransferBlueprint` type-class instance for `ArbitTransfer` for a given proposition.
   * @tparam P the type of proposition on the arbit transfer
   * @return a type-class instance of `ValueTransferBlueprint` for `ArbitTransfer`
   */
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

  /**
   * Defines a `ValueTransferBlueprint` type-class instance for `AssetTransfer` for a given proposition.
   * @tparam P the type of proposition on the asset transfer
   * @return a type-class instance of `ValueTransferBlueprint` for `AssetTransfer`
   */
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
