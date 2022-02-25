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

  private def mergeInputsWithAttestation[D <: DionProposition: EvidenceProducer](
    inputs:      IndexedSeq[(Address, DionBox.Nonce)],
    attestation: ListMap[D, DionProof[D]]
  ): Either[ToTetraTxFailure, ListMap[BoxReference, (Proposition, Proof)]] =
    for {
      dionAddressInputs <-
        inputs.foldLeft[Either[ToTetraTxFailure, Chain[BoxReference]]](Chain.empty.asRight) {
          case (Right(validInputs), input) =>
            input._1.toDionAddress
              .map(dionAddress => dionAddress -> input._2)
              .map(validInputs.append)
              .leftMap(error => ToTetraTxFailures.InvalidAddress(input._1, error))
          case (error, _) => error
        }
      merged <-
        attestation.foldLeft(ListMap.empty[BoxReference, (Proposition, Proof)].asRight[ToTetraTxFailure]) {
          case (Right(result), (proposition, proof)) =>
            for {
              propAndProof <-
                (proposition, proof) match {
                  case (prop: PublicKeyPropositionCurve25519, proof: SignatureCurve25519) =>
                    toCurvePropAndProof(prop, proof)
                  case (prop: PublicKeyPropositionEd25519, proof: SignatureEd25519) =>
                    toEdPropAndProof(prop, proof)
                  case (prop: ThresholdPropositionCurve25519, proof: ThresholdSignatureCurve25519) =>
                    toThresholdPropAndProof(prop, proof)
                  case (x, _) => ToTetraTxFailures.InvalidPropositionFailures.InvalidProposition(x).asLeft
                }
              mergedInputs =
                dionAddressInputs
                  .filter(_._1.typedEvidence == propAndProof._1.typedEvidence)
                  .map(_ -> propAndProof)
            } yield result ++ ListMap.from(mergedInputs.iterator)
          case (error, _) => error
        }
    } yield merged

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

  private def toThresholdPropAndProof(
    dionProp:  ThresholdPropositionCurve25519,
    dionProof: ThresholdSignatureCurve25519
  ): Either[ToTetraTxFailure, (Proposition, Proof)] =
    for {
      curveProps <-
        dionProp.pubKeyProps
          .foldLeft[Either[ToTetraTxFailure, Chain[Propositions.Knowledge.Curve25519]]](Chain.empty.asRight) {
            case (Right(validProps), prop) =>
              Sized
                .strict[Bytes, VerificationKeys.Curve25519.Length](Bytes(prop.pubKeyBytes.value))
                .map(pubKeyBytes => Propositions.Knowledge.Curve25519(VerificationKeys.Curve25519(pubKeyBytes)))
                .map(validProps.append)
                .leftMap(error => ToTetraTxFailures.InvalidPropositionFailures.InvalidPropositionSize(prop, error))
            case (error, _) => error
          }
      curveProofs <-
        dionProof.signatures
          .foldLeft[Either[ToTetraTxFailure, Chain[Proofs.Knowledge.Curve25519]]](Chain.empty.asRight) {
            case (Right(validProofs), proof) =>
              Sized
                .strict[Bytes, Proofs.Knowledge.Curve25519.Length](Bytes(proof.sigBytes.value))
                .map(proofBytes => Proofs.Knowledge.Curve25519(proofBytes))
                .map(validProofs.append)
                .leftMap(error => ToTetraTxFailures.InvalidProofSize(proof, error))
            case (error, _) => error
          }
      thresholdProp = Propositions.Compositional.Threshold(dionProp.threshold, ListSet.from(curveProps.iterator))
      thresholdProof = Proofs.Compositional.Threshold(curveProofs.toList)
    } yield thresholdProp -> thresholdProof

  private def toData(data: Option[DionLatin1Data]): Either[ToTetraTxFailure, Option[TransactionData]] =
    data.fold[Either[ToTetraTxFailure, Option[TransactionData]]](none.asRight)(d =>
      Sized
        .max[Latin1Data, Lengths.`127`.type](Latin1Data.fromData(d.value))
        .map(_.some)
        .leftMap(error => ToTetraTxFailures.InvalidData(d, error))
    )

  private def toFeeValue(fee: DionInt128): Either[ToTetraTxFailure, Int128] =
    Sized.max[BigInt, Lengths.`128`.type](fee.toLong).leftMap(error => ToTetraTxFailures.InvalidFee(fee, error))

  private def toFeeOutput(
    address: Address,
    value:   TokenValueHolder
  ): Either[ToTetraTxFailure, Option[Transaction.PolyOutput]] =
    value match {
      case simpleValue: SimpleValue =>
        if (value.quantity > 0) toPolyOutput(address, simpleValue).map(_.some) else none.asRight
      case invalid => ToTetraTxFailures.InvalidFeeOutput(address, value).asLeft
    }

  private def toPolyCoinOutputs(
    from: IndexedSeq[(Address, TokenValueHolder)]
  ): Either[ToTetraTxFailure, NonEmptyChain[Transaction.CoinOutput]] =
    from
      .foldLeft[Either[ToTetraTxFailure, NonEmptyChain[Transaction.CoinOutput]]](
        ToTetraTxFailures.EmptyOutputs.asLeft
      ) {
        case (Right(coinOutputs), (address: Address, simpleValue: SimpleValue)) =>
          toPolyOutput(address, simpleValue).map(coinOutputs.append)
        case (Right(_), invalid) => ToTetraTxFailures.InvalidOutput(invalid._1, invalid._2).asLeft
        case (Left(ToTetraTxFailures.EmptyOutputs), (address, simpleValue: SimpleValue)) =>
          toPolyOutput(address, simpleValue).map(NonEmptyChain.one)
        case (error, _) => error
      }

  private def toArbitCoinOutputs(
    from: IndexedSeq[(Address, TokenValueHolder)]
  ): Either[ToTetraTxFailure, NonEmptyChain[Transaction.CoinOutput]] =
    from
      .foldLeft[Either[ToTetraTxFailure, NonEmptyChain[Transaction.CoinOutput]]](
        ToTetraTxFailures.EmptyOutputs.asLeft
      ) {
        case (Right(coinOutputs), (address: Address, simpleValue: SimpleValue)) =>
          toArbitOutput(address, simpleValue).map(coinOutputs.append)
        case (Right(_), invalid) => ToTetraTxFailures.InvalidOutput(invalid._1, invalid._2).asLeft
        case (Left(ToTetraTxFailures.EmptyOutputs), (address, simpleValue: SimpleValue)) =>
          toArbitOutput(address, simpleValue).map(NonEmptyChain.one)
        case (error, _) => error
      }

  private def toAssetCoinOutputs(
    from: IndexedSeq[(Address, TokenValueHolder)]
  ): Either[ToTetraTxFailure, NonEmptyChain[Transaction.CoinOutput]] =
    from
      .foldLeft[Either[ToTetraTxFailure, NonEmptyChain[Transaction.CoinOutput]]](
        ToTetraTxFailures.EmptyOutputs.asLeft
      ) {
        case (Right(coinOutputs), (address: Address, assetValue: AssetValue)) =>
          toAssetOutput(address, assetValue).map(coinOutputs.append)
        case (Right(_), invalid) => ToTetraTxFailures.InvalidOutput(invalid._1, invalid._2).asLeft
        case (Left(ToTetraTxFailures.EmptyOutputs), (address, assetValue: AssetValue)) =>
          toAssetOutput(address, assetValue).map(NonEmptyChain.one)
        case (error, _) => error
      }

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
