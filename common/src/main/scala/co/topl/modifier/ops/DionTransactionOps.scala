package co.topl.modifier.ops

import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import co.topl.attestation.ops.AddressOps.ToDionAddressFailure
import co.topl.attestation.ops.implicits._
import co.topl.attestation.{
  Address,
  EvidenceProducer,
  Proof => DionProof,
  Proposition => DionProposition,
  PublicKeyPropositionCurve25519,
  PublicKeyPropositionEd25519,
  SignatureCurve25519,
  SignatureEd25519,
  ThresholdPropositionCurve25519,
  ThresholdSignatureCurve25519
}
import co.topl.models._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}
import co.topl.modifier.box.{AssetValue, Box => DionBox, SimpleValue, TokenValueHolder}
import co.topl.modifier.ops.AssetValueOps.implicits._
import co.topl.modifier.ops.SimpleValueOps.implicits._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction => DionTransaction}
import co.topl.typeclasses.implicits._
import co.topl.utils.StringDataTypes.{Latin1Data => DionLatin1Data}
import co.topl.utils.{Int128 => DionInt128}

import scala.collection.immutable.{ListMap, ListSet}
import scala.language.implicitConversions

class DionTransactionOps[P <: DionProposition](val transaction: DionTransaction[_, P]) extends AnyVal {

  import DionTransactionOps._

  /**
   * Attempts to upgrade a Dion [[DionTransaction.TX]] to an equivalent Tetra [[Transaction]].
   *
   * @param evidenceProducer an instance of the [[EvidenceProducer]] typeclass.
   * @return a [[Transaction]] if the upgrade is successful, otherwise a [[ToTetraTxFailure]]
   */
  def toTetraTx(implicit evidenceProducer: EvidenceProducer[P]): Either[ToTetraTxFailure, Transaction] =
    transaction match {
      case transfer: ArbitTransfer[P] =>
        for {
          data          <- toData(transfer.data)
          dionFeeOutput <- transfer.to.headOption.toRight(ToTetraTxFailures.EmptyOutputs)
          feeOutput     <- toFeeOutput(dionFeeOutput._1, dionFeeOutput._2)
          feeValue      <- toFeeValue(transfer.fee)
          arbitOutputs  <- toArbitCoinOutputs(transfer.to.tail)
          inputs        <- mergeInputsWithAttestation(transfer.from, transfer.attestation)
        } yield Transaction(
          inputs,
          feeOutput,
          arbitOutputs,
          feeValue,
          transfer.timestamp,
          data,
          transfer.minting
        )
      case transfer: AssetTransfer[P] =>
        for {
          data          <- toData(transfer.data)
          dionFeeOutput <- transfer.to.headOption.toRight(ToTetraTxFailures.EmptyOutputs)
          feeOutput     <- toFeeOutput(dionFeeOutput._1, dionFeeOutput._2)
          feeValue      <- toFeeValue(transfer.fee)
          assetOutputs  <- toAssetCoinOutputs(transfer.to.tail)
          inputs        <- mergeInputsWithAttestation(transfer.from, transfer.attestation)
        } yield Transaction(
          inputs,
          feeOutput,
          assetOutputs,
          feeValue,
          transfer.timestamp,
          data,
          transfer.minting
        )
      case transfer: PolyTransfer[P] =>
        for {
          data          <- toData(transfer.data)
          dionFeeOutput <- transfer.to.headOption.toRight(ToTetraTxFailures.EmptyOutputs)
          feeOutput     <- toFeeOutput(dionFeeOutput._1, dionFeeOutput._2)
          feeValue      <- toFeeValue(transfer.fee)
          polyOutputs   <- toPolyCoinOutputs(transfer.to.tail)
          inputs        <- mergeInputsWithAttestation(transfer.from, transfer.attestation)
        } yield Transaction(
          inputs,
          feeOutput,
          polyOutputs,
          feeValue,
          transfer.timestamp,
          data,
          transfer.minting
        )
    }

  /**
   * Merges a Dion [[DionTransaction.TX]] input set with an attestation map.
   * @param inputs the set of inputs to merge
   * @param attestation the attestation map to merge
   * @tparam D a [[DionProposition]] type with an instance of [[EvidenceProducer]]
   * @return a Tetra [[Transaction]] compatible input set if successful, otherwise a [[ToTetraTxFailure]]
   */
  private def mergeInputsWithAttestation[D <: DionProposition: EvidenceProducer](
    inputs:      IndexedSeq[(Address, DionBox.Nonce)],
    attestation: ListMap[D, DionProof[D]]
  ): Either[ToTetraTxFailure, ListMap[BoxReference, (Proposition, Proof)]] =
    for {
      dionAddressInputs <-
        Chain
          .fromSeq(inputs)
          .traverse(input =>
            input._1.toDionAddress
              .map(dionAddress => dionAddress -> input._2)
              .leftMap(error => ToTetraTxFailures.InvalidAddress(input._1, error))
          )
      tetraPropsAndProofs <-
        Chain
          .fromSeq(attestation.toSeq)
          .traverse {
            case (prop: PublicKeyPropositionCurve25519, proof: SignatureCurve25519) =>
              toCurvePropAndProof(prop, proof)
            case (prop: PublicKeyPropositionEd25519, proof: SignatureEd25519) =>
              toEdPropAndProof(prop, proof)
            case (prop: ThresholdPropositionCurve25519, proof: ThresholdSignatureCurve25519) =>
              toThresholdPropAndProof(prop, proof)
            case (x, _) => ToTetraTxFailures.InvalidPropositionFailures.InvalidProposition(x).asLeft
          }
      merged <-
        dionAddressInputs.traverse(input =>
          tetraPropsAndProofs
            .find(pair => pair._1.typedEvidence == input._1.typedEvidence)
            .toRight(ToTetraTxFailures.MissingProof(input))
            .map(input -> _)
        )
    } yield ListMap.from(merged.iterator)

  /**
   * Converts a Dion [[PublicKeyPropositionCurve25519]] and [[SignatureCurve25519]] pair to an equivalent
   * Tetra [[Proposition]] and [[Proof]]
   * @param dionProp the Dion proposition to convert
   * @param dionProof the Dion proof to convert
   * @return a [[Proposition]] and [[Proof]] pair if successful, otherwise a [[ToTetraTxFailure]]
   */
  private def toCurvePropAndProof(
    dionProp:  PublicKeyPropositionCurve25519,
    dionProof: SignatureCurve25519
  ): Either[ToTetraTxFailure, (Proposition, Proof)] =
    for {
      propBytes <-
        Sized
          .strict[Bytes, VerificationKeys.Curve25519.Length](Bytes(dionProp.pubKeyBytes.value))
          .leftMap(error => ToTetraTxFailures.InvalidPropositionFailures.InvalidPropositionSize(dionProp, error))
      proofBytes <-
        Sized
          .strict[Bytes, Proofs.Knowledge.Curve25519.Length](Bytes(dionProof.sigBytes.value))
          .leftMap(error => ToTetraTxFailures.InvalidProofSize(dionProof, error))
      curveProp = Propositions.Knowledge.Curve25519(VerificationKeys.Curve25519(propBytes))
      curveProof = Proofs.Knowledge.Curve25519(proofBytes)
    } yield curveProp -> curveProof

  /**
   * Converts a Dion [[PublicKeyPropositionEd25519]] and [[SignatureEd25519]] pair to an equivalent
   * Tetra [[Proposition]] and [[Proof]]
   * @param dionProp the Dion proposition to convert
   * @param dionProof the Dion proof to convert
   * @return a [[Proposition]] and [[Proof]] pair if successful, otherwise a [[ToTetraTxFailure]]
   */
  private def toEdPropAndProof(
    dionProp:  PublicKeyPropositionEd25519,
    dionProof: SignatureEd25519
  ): Either[ToTetraTxFailure, (Proposition, Proof)] =
    for {
      propBytes <-
        Sized
          .strict[Bytes, VerificationKeys.Ed25519.Length](Bytes(dionProp.pubKeyBytes.value))
          .leftMap(error => ToTetraTxFailures.InvalidPropositionFailures.InvalidPropositionSize(dionProp, error))
      proofBytes <-
        Sized
          .strict[Bytes, Proofs.Knowledge.Ed25519.Length](Bytes(dionProof.sigBytes.value))
          .leftMap(error => ToTetraTxFailures.InvalidProofSize(dionProof, error))
      edProp = Propositions.Knowledge.Ed25519(VerificationKeys.Ed25519(propBytes))
      edProof = Proofs.Knowledge.Ed25519(proofBytes)
    } yield edProp -> edProof

  /**
   * Converts a Dion [[ThresholdPropositionCurve25519]] and [[ThresholdSignatureCurve25519]] pair to an equivalent
   * Tetra [[Proposition]] and [[Proof]]
   * @param dionProp the Dion proposition to convert
   * @param dionProof the Dion proof to convert
   * @return a [[Proposition]] and [[Proof]] pair if successful, otherwise a [[ToTetraTxFailure]]
   */
  private def toThresholdPropAndProof(
    dionProp:  ThresholdPropositionCurve25519,
    dionProof: ThresholdSignatureCurve25519
  ): Either[ToTetraTxFailure, (Proposition, Proof)] =
    for {
      curveProps <-
        Chain
          .fromSeq(dionProp.pubKeyProps.toSeq)
          .traverse(prop =>
            Sized
              .strict[Bytes, VerificationKeys.Curve25519.Length](Bytes(prop.pubKeyBytes.value))
              .map(pubKeyBytes => Propositions.Knowledge.Curve25519(VerificationKeys.Curve25519(pubKeyBytes)))
              .leftMap(error => ToTetraTxFailures.InvalidPropositionFailures.InvalidPropositionSize(prop, error))
          )
      curveProofs <-
        Chain
          .fromSeq(dionProof.signatures.toSeq)
          .traverse(proof =>
            Sized
              .strict[Bytes, Proofs.Knowledge.Curve25519.Length](Bytes(proof.sigBytes.value))
              .map(proofBytes => Proofs.Knowledge.Curve25519(proofBytes))
              .leftMap(error => ToTetraTxFailures.InvalidProofSize(proof, error))
          )
      thresholdProp = Propositions.Compositional.Threshold(dionProp.threshold, ListSet.from(curveProps.iterator))
      thresholdProof = Proofs.Compositional.Threshold(curveProofs.toList)
    } yield thresholdProp -> thresholdProof

  private def toData(data: Option[DionLatin1Data]): Either[ToTetraTxFailure, Option[TransactionData]] =
    data.traverse(d =>
      Sized
        .max[Latin1Data, Lengths.`127`.type](Latin1Data.fromData(d.value))
        .leftMap(error => ToTetraTxFailures.InvalidData(d, error))
    )

  private def toFeeValue(fee: DionInt128): Either[ToTetraTxFailure, Int128] =
    Sized.max[BigInt, Lengths.`128`.type](fee.toLong).leftMap(error => ToTetraTxFailures.InvalidFee(fee, error))

  /**
   * Attempts to convert a Dion fee output value into an optional [[Transaction.PolyOutput]].
   * The resulting value will be [[None]] if the quantity of the input value is 0.
   * @param address the address to use in the fee output
   * @param value the value to use in the fee output
   * @return an optional [[Transaction.PolyOutput]] if the conversion is successful, otherwise a [[ToTetraTxFailure]]
   */
  private def toFeeOutput(
    address: Address,
    value:   TokenValueHolder
  ): Either[ToTetraTxFailure, Option[Transaction.PolyOutput]] =
    value match {
      case simpleValue: SimpleValue =>
        if (value.quantity > 0) toPolyOutput(address, simpleValue).map(_.some) else none.asRight
      case invalid => ToTetraTxFailures.InvalidFeeOutput(address, invalid).asLeft
    }

  private def toPolyCoinOutputs(
    from: IndexedSeq[(Address, TokenValueHolder)]
  ): Either[ToTetraTxFailure, NonEmptyChain[Transaction.CoinOutput]] =
    NonEmptyChain
      .fromSeq(from)
      .toRight(ToTetraTxFailures.EmptyOutputs)
      .flatMap(_.traverse {
        case (address: Address, simpleValue: SimpleValue) => toPolyOutput(address, simpleValue)
        case (invalidAddress, invalidToken) => ToTetraTxFailures.InvalidOutput(invalidAddress, invalidToken).asLeft
      })

  private def toArbitCoinOutputs(
    from: IndexedSeq[(Address, TokenValueHolder)]
  ): Either[ToTetraTxFailure, NonEmptyChain[Transaction.CoinOutput]] =
    NonEmptyChain
      .fromSeq(from)
      .toRight(ToTetraTxFailures.EmptyOutputs)
      .flatMap(_.traverse {
        case (address: Address, simpleValue: SimpleValue) => toArbitOutput(address, simpleValue)
        case (invalidAddress, invalidToken) => ToTetraTxFailures.InvalidOutput(invalidAddress, invalidToken).asLeft
      })

  private def toAssetCoinOutputs(
    from: IndexedSeq[(Address, TokenValueHolder)]
  ): Either[ToTetraTxFailure, NonEmptyChain[Transaction.CoinOutput]] =
    NonEmptyChain
      .fromSeq(from)
      .toRight(ToTetraTxFailures.EmptyOutputs)
      .flatMap(_.traverse {
        case (address: Address, assetValue: AssetValue) => toAssetOutput(address, assetValue)
        case (invalidAddress, invalidToken) => ToTetraTxFailures.InvalidOutput(invalidAddress, invalidToken).asLeft
      })

  private def toPolyOutput(
    address:     Address,
    simpleValue: SimpleValue
  ): Either[ToTetraTxFailure, Transaction.PolyOutput] =
    for {
      dionAddress <-
        address.toDionAddress
          .leftMap(error => ToTetraTxFailures.InvalidAddress(address, error))
      polyOutput <-
        simpleValue
          .toPolyOutput(dionAddress)
          .leftMap(_ => ToTetraTxFailures.InvalidOutput(address, simpleValue))
    } yield polyOutput

  private def toArbitOutput(
    address:     Address,
    simpleValue: SimpleValue
  ): Either[ToTetraTxFailure, Transaction.ArbitOutput] =
    for {
      dionAddress <-
        address.toDionAddress
          .leftMap(error => ToTetraTxFailures.InvalidAddress(address, error))
      arbitOutput <-
        simpleValue
          .toArbitOutput(dionAddress)
          .leftMap(_ => ToTetraTxFailures.InvalidOutput(address, simpleValue))
    } yield arbitOutput

  private def toAssetOutput(
    address:    Address,
    assetValue: AssetValue
  ): Either[ToTetraTxFailure, Transaction.AssetOutput] =
    for {
      dionAddress <-
        address.toDionAddress
          .leftMap(error => ToTetraTxFailures.InvalidAddress(address, error))
      assetOutput <-
        assetValue
          .toAssetOutput(dionAddress)
          .leftMap(_ => ToTetraTxFailures.InvalidOutput(address, assetValue))
    } yield assetOutput
}

object DionTransactionOps {

  sealed trait ToTetraTxFailure

  object ToTetraTxFailures {
    case object EmptyOutputs extends ToTetraTxFailure
    case class InvalidValueQuantity(value: TokenValueHolder, inner: Sized.InvalidLength) extends ToTetraTxFailure
    case class InvalidOutput(address: Address, value: TokenValueHolder) extends ToTetraTxFailure
    case class InvalidAddress(address: Address, inner: ToDionAddressFailure) extends ToTetraTxFailure
    case class InvalidData(data: DionLatin1Data, inner: Sized.InvalidLength) extends ToTetraTxFailure
    case class InvalidFee(fee: DionInt128, inner: Sized.InvalidLength) extends ToTetraTxFailure
    case class InvalidFeeOutput(address: Address, value: TokenValueHolder) extends ToTetraTxFailure
    case class MissingProof(input: (DionAddress, BoxNonce)) extends ToTetraTxFailure

    case class InvalidProofSize(proof: DionProof[_ <: DionProposition], inner: Sized.InvalidLength)
        extends ToTetraTxFailure

    sealed trait InvalidPropositionFailure extends ToTetraTxFailure

    object InvalidPropositionFailures {
      case class PropositionCoerceFailure(reason: String) extends InvalidPropositionFailure
      case class InvalidProposition(proposition: DionProposition) extends InvalidPropositionFailure

      case class InvalidPropositionSize(proposition: DionProposition, inner: Sized.InvalidLength)
          extends InvalidPropositionFailure
    }
  }

  trait ToDionTransactionOps {

    implicit def dionTransactionOpsFromDionTransaction[P <: DionProposition: EvidenceProducer](
      transaction: DionTransaction[_, P]
    ): DionTransactionOps[P] =
      new DionTransactionOps(transaction)
  }

  trait Implicits extends ToDionTransactionOps

  object implicits extends Implicits
}
