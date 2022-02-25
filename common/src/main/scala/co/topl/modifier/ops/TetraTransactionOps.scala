package co.topl.modifier.ops

import cats.data.Chain
import cats.implicits._
import co.topl.attestation._
import co.topl.crypto.{PublicKey, Signature}
import co.topl.models.{DionAddress, Proof, Proofs, Proposition, Propositions, Transaction}
import co.topl.modifier.box._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction => DionTransaction}
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

import scala.collection.SortedSet
import scala.collection.immutable.ListMap
import scala.language.implicitConversions

class TetraTransactionOps(val transaction: Transaction) extends AnyVal {
  import TetraTransactionOps._

  def toDionTx: Either[ToDionTxFailure, DionTransaction.TX] =
    for {
      expectedProposition <- transaction.inputs.headOption.toRight(ToDionTxFailures.NoInputs)
      tx <-
        (expectedProposition._2._1, transaction.coinOutputs.head) match {
          case (_: Propositions.Knowledge.Curve25519, _: Transaction.PolyOutput)     => asPolyTransferCurve
          case (_: Propositions.Knowledge.Curve25519, _: Transaction.ArbitOutput)    => asArbitTransferCurve
          case (_: Propositions.Knowledge.Curve25519, _: Transaction.AssetOutput)    => asAssetTransferCurve
          case (_: Propositions.Knowledge.Ed25519, _: Transaction.PolyOutput)        => asPolyTransferEd
          case (_: Propositions.Knowledge.Ed25519, _: Transaction.ArbitOutput)       => asArbitTransferEd
          case (_: Propositions.Knowledge.Ed25519, _: Transaction.AssetOutput)       => asAssetTransferEd
          case (_: Propositions.Compositional.Threshold, _: Transaction.PolyOutput)  => asPolyTransferThreshold
          case (_: Propositions.Compositional.Threshold, _: Transaction.ArbitOutput) => asArbitTransferThreshold
          case (_: Propositions.Compositional.Threshold, _: Transaction.AssetOutput) => asAssetTransferThreshold
          case (prop, output) => ToDionTxFailures.IllegalTransferType(prop, output).asLeft[DionTransaction.TX]
        }
    } yield tx

  private def asPolyTransferCurve: Either[ToDionTxFailure, PolyTransfer[PublicKeyPropositionCurve25519]] =
    for {
      attestation <- curveAttestation
      coinOutputs <- polyOutputs
      tx =
        PolyTransfer(
          getFrom,
          getFeeOutput.fold(coinOutputs)(coinOutputs.prepend).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  private def asPolyTransferEd: Either[ToDionTxFailure, PolyTransfer[PublicKeyPropositionEd25519]] =
    for {
      attestation <- edAttestation
      coinOutputs <- polyOutputs
      tx =
        PolyTransfer(
          getFrom,
          getFeeOutput.fold(coinOutputs)(coinOutputs.prepend).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  private def asPolyTransferThreshold: Either[ToDionTxFailure, PolyTransfer[ThresholdPropositionCurve25519]] =
    for {
      attestation <- thresholdAttestation
      coinOutputs <- polyOutputs
      tx =
        PolyTransfer(
          getFrom,
          getFeeOutput.fold(coinOutputs)(coinOutputs.prepend).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  private def asArbitTransferCurve: Either[ToDionTxFailure, ArbitTransfer[PublicKeyPropositionCurve25519]] =
    for {
      attestation <- curveAttestation
      coinOutputs <- arbitOutputs
      tx =
        ArbitTransfer(
          getFrom,
          getFeeOutput.fold(coinOutputs)(coinOutputs.prepend).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  private def asArbitTransferEd: Either[ToDionTxFailure, ArbitTransfer[PublicKeyPropositionEd25519]] =
    for {
      attestation <- edAttestation
      coinOutputs <- arbitOutputs
      tx =
        ArbitTransfer(
          getFrom,
          getFeeOutput.fold(coinOutputs)(coinOutputs.prepend).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  private def asArbitTransferThreshold: Either[ToDionTxFailure, ArbitTransfer[ThresholdPropositionCurve25519]] =
    for {
      attestation <- thresholdAttestation
      coinOutputs <- arbitOutputs
      tx =
        ArbitTransfer(
          getFrom,
          getFeeOutput.fold(coinOutputs)(coinOutputs.prepend).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  private def asAssetTransferCurve: Either[ToDionTxFailure, AssetTransfer[PublicKeyPropositionCurve25519]] =
    for {
      attestation <- curveAttestation
      coinOutputs <- assetOutputs
      tx =
        AssetTransfer(
          getFrom,
          getFeeOutput.fold(coinOutputs)(coinOutputs.prepend).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  private def asAssetTransferEd: Either[ToDionTxFailure, AssetTransfer[PublicKeyPropositionEd25519]] =
    for {
      attestation <- edAttestation
      coinOutputs <- assetOutputs
      tx =
        AssetTransfer(
          getFrom,
          getFeeOutput.fold(coinOutputs)(coinOutputs.prepend).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  private def asAssetTransferThreshold: Either[ToDionTxFailure, AssetTransfer[ThresholdPropositionCurve25519]] =
    for {
      attestation <- thresholdAttestation
      coinOutputs <- assetOutputs
      tx =
        AssetTransfer(
          getFrom,
          getFeeOutput.fold(coinOutputs)(coinOutputs.prepend).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  private def curveAttestation: Either[ToDionTxFailure, ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]] =
    transaction.inputs.values
      .foldLeft(
        ListMap.empty[PublicKeyPropositionCurve25519, SignatureCurve25519].asRight[ToDionTxFailure]
      ) {
        case (Right(attMap), (prop: Propositions.Knowledge.Curve25519, proof: Proofs.Knowledge.Curve25519)) =>
          attMap
            .updated(
              PublicKeyPropositionCurve25519(PublicKey(prop.key.bytes.data.toArray)),
              SignatureCurve25519(Signature(proof.bytes.data.toArray))
            )
            .asRight
        case (Right(_), illegal) => ToDionTxFailures.IllegalProposition(illegal._1).asLeft
        case (error, _)          => error
      }

  private def edAttestation: Either[ToDionTxFailure, ListMap[PublicKeyPropositionEd25519, SignatureEd25519]] =
    transaction.inputs.values
      .foldLeft(
        ListMap.empty[PublicKeyPropositionEd25519, SignatureEd25519].asRight[ToDionTxFailure]
      ) {
        case (Right(attMap), (prop: Propositions.Knowledge.Ed25519, proof: Proofs.Knowledge.Ed25519)) =>
          attMap
            .updated(
              PublicKeyPropositionEd25519(PublicKey(prop.key.bytes.data.toArray)),
              SignatureEd25519(Signature(proof.bytes.data.toArray))
            )
            .asRight
        case (Right(_), illegal) => ToDionTxFailures.IllegalProposition(illegal._1).asLeft
        case (error, _)          => error
      }

  private def thresholdAttestation
    : Either[ToDionTxFailure, ListMap[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]] =
    transaction.inputs.values
      .foldLeft(
        ListMap.empty[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519].asRight[ToDionTxFailure]
      ) {
        case (Right(attMap), (prop: Propositions.Compositional.Threshold, proof: Proofs.Compositional.Threshold)) =>
          for {
            props <-
              prop.propositions.foldLeft(List.empty[PublicKeyPropositionCurve25519].asRight[ToDionTxFailure]) {
                case (Right(props), p: Propositions.Knowledge.Curve25519) =>
                  (props :+ PublicKeyPropositionCurve25519(PublicKey(p.key.bytes.data.toArray))).asRight
                case (Right(_), illegal) => ToDionTxFailures.IllegalProposition(illegal).asLeft
                case (error, _)          => error
              }
            proofs <-
              proof.proofs.foldLeft(List.empty[SignatureCurve25519].asRight[ToDionTxFailure]) {
                case (Right(proofs), p: Proofs.Knowledge.Curve25519) =>
                  (proofs :+ SignatureCurve25519(Signature(p.bytes.data.toArray))).asRight
                case (Right(_), illegal) => ToDionTxFailures.IllegalProof(illegal).asLeft
                case (error, _)          => error
              }
          } yield attMap.updated(
            ThresholdPropositionCurve25519(prop.threshold, SortedSet.from(props)),
            ThresholdSignatureCurve25519(proofs.toSet)
          )
        case (Right(_), illegal) => ToDionTxFailures.IllegalProposition(illegal._1).asLeft
        case (error, _)          => error
      }

  private def polyOutputs: Either[ToDionTxFailure, Chain[(Address, SimpleValue)]] =
    transaction.coinOutputs
      .foldLeft(Chain.empty[(Address, SimpleValue)].asRight[ToDionTxFailure]) {
        case (Right(outputs), polyOutput: Transaction.PolyOutput) =>
          outputs.append(toAddress(polyOutput.dionAddress) -> SimpleValue(Int128(polyOutput.value.data))).asRight
        case (Right(_), coin) => ToDionTxFailures.InvalidOutput(coin, "must be Poly token type").asLeft
        case (error, _)       => error
      }

  private def arbitOutputs: Either[ToDionTxFailure, Chain[(Address, SimpleValue)]] =
    transaction.coinOutputs
      .foldLeft(Chain.empty[(Address, SimpleValue)].asRight[ToDionTxFailure]) {
        case (Right(outputs), arbitOutput: Transaction.ArbitOutput) =>
          outputs.append(toAddress(arbitOutput.dionAddress) -> SimpleValue(Int128(arbitOutput.value.data))).asRight
        case (Right(_), coin) => ToDionTxFailures.InvalidOutput(coin, "must be Arbit token type").asLeft
        case (error, _)       => error
      }

  private def assetOutputs: Either[ToDionTxFailure, Chain[(Address, TokenValueHolder)]] =
    transaction.coinOutputs
      .foldLeft(Chain.empty[(Address, AssetValue)].asRight[ToDionTxFailure]) {
        case (Right(outputs), assetOutput: Transaction.AssetOutput) =>
          outputs
            .append(
              toAddress(assetOutput.dionAddress) -> AssetValue(
                Int128(assetOutput.value.quantity.data),
                AssetCode(
                  assetOutput.value.assetCode.version,
                  toAddress(assetOutput.value.assetCode.issuer),
                  Latin1Data.fromData(assetOutput.value.assetCode.shortName.data.bytes)
                ),
                SecurityRoot(assetOutput.value.securityRoot.toArray),
                assetOutput.value.metadata.map(data => Latin1Data.fromData(data.data.bytes))
              )
            )
            .asRight
        case (Right(_), coin) => ToDionTxFailures.InvalidOutput(coin, "must be Asset token type").asLeft
        case (error, _)       => error
      }

  private def getFrom: IndexedSeq[(Address, Box.Nonce)] =
    transaction.inputs.keys.map(_.bimap(toAddress, x => x)).toIndexedSeq

  private def getFee: Int128 = Int128(transaction.fee.data)

  private def getData: Option[Latin1Data] = transaction.data.map(d => Latin1Data.fromData(d.data.bytes))

  private def getFeeOutput: Option[(Address, SimpleValue)] =
    transaction.feeOutput.map(fee => toAddress(fee.dionAddress) -> SimpleValue(Int128(fee.value.data)))

  private def toAddress(dionAddress: DionAddress): Address =
    Address(Evidence(dionAddress.typedEvidence.allBytes.toArray))(dionAddress.networkPrefix.value)
}

object TetraTransactionOps {

  sealed trait ToDionTxFailure

  object ToDionTxFailures {
    case class InvalidOutput(invalidOutput: Transaction.CoinOutput, reason: String) extends ToDionTxFailure
    case class IllegalProposition(propositon: Proposition) extends ToDionTxFailure
    case class IllegalProof(proof: Proof) extends ToDionTxFailure
    case class IllegalTransferType(proposition: Proposition, coinType: Transaction.CoinOutput) extends ToDionTxFailure
    case object NoInputs extends ToDionTxFailure
  }

  trait ToTetraTransactionOps {

    implicit def tetraTransactionOpsFromTetraTransaction(transaction: Transaction): TetraTransactionOps =
      new TetraTransactionOps(transaction)
  }

  trait Implicits extends ToTetraTransactionOps

  object implicits extends Implicits
}
