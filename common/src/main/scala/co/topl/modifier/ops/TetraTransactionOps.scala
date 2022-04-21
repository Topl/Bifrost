package co.topl.modifier.ops

import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.attestation._
import co.topl.crypto.{PublicKey, Signature}
import co.topl.models.{DionAddress, Proof, Proofs, Proposition, Propositions, Transaction}
import co.topl.modifier.box.{AssetCode, AssetValue, Box, SecurityRoot, SimpleValue, TokenValueHolder}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction => DionTransaction}
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

import scala.collection.SortedSet
import scala.collection.immutable.ListMap
import scala.language.implicitConversions

class TetraTransactionOps(private val transaction: Transaction) extends AnyVal {
  import TetraTransactionOps._

  /**
   * Attempts to convert a Tetra [[Transaction]] into an equivalent [[DionTransaction.TX]] value.
   * @return if successful, a [[DionTransaction.TX]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  def toDionTx: ToDionTxResult[DionTransaction.TX] =
    for {
      // all propositions in the Dion transfer must be the same as the first one
      expectedProposition <-
        transaction.inputs.headOption.map(_._2._1).toRight(ToDionTxFailures.EmptyInputs)
      tx <-
        // use the first proposition type and first coin output type to derive what the Dion transfer type will be
        (expectedProposition, transaction.coinOutputs.head) match {
          case (_: Propositions.Knowledge.Curve25519, _: Transaction.PolyOutput)     => asPolyTransferCurve
          case (_: Propositions.Knowledge.Curve25519, _: Transaction.ArbitOutput)    => asArbitTransferCurve
          case (_: Propositions.Knowledge.Curve25519, _: Transaction.AssetOutput)    => asAssetTransferCurve
          case (_: Propositions.Knowledge.Ed25519, _: Transaction.PolyOutput)        => asPolyTransferEd
          case (_: Propositions.Knowledge.Ed25519, _: Transaction.ArbitOutput)       => asArbitTransferEd
          case (_: Propositions.Knowledge.Ed25519, _: Transaction.AssetOutput)       => asAssetTransferEd
          case (_: Propositions.Compositional.Threshold, _: Transaction.PolyOutput)  => asPolyTransferThreshold
          case (_: Propositions.Compositional.Threshold, _: Transaction.ArbitOutput) => asArbitTransferThreshold
          case (_: Propositions.Compositional.Threshold, _: Transaction.AssetOutput) => asAssetTransferThreshold
          case (prop, output) => ToDionTxFailures.InvalidTransferType(prop, output).asLeft[DionTransaction.TX]
        }
    } yield tx

  /**
   * Attempt to convert the Tetra transaction into a [[PolyTransfer]] with proposition type
   * [[PublicKeyPropositionCurve25519]].
   * @return if successful, a [[PolyTransfer]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  private def asPolyTransferCurve: ToDionTxResult[PolyTransfer[PublicKeyPropositionCurve25519]] =
    for {
      attestation <- curveAttestation
      coinOutputs <- polyOutputs
      tx =
        PolyTransfer(
          getFrom,
          coinOutputs.prepend(getFeeOutput).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  /**
   * Attempt to convert the Tetra transaction into a [[PolyTransfer]] with proposition type
   * [[PublicKeyPropositionEd25519]].
   * @return if successful, a [[PolyTransfer]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  private def asPolyTransferEd: ToDionTxResult[PolyTransfer[PublicKeyPropositionEd25519]] =
    for {
      attestation <- edAttestation
      coinOutputs <- polyOutputs
      tx =
        PolyTransfer(
          getFrom,
          coinOutputs.prepend(getFeeOutput).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  /**
   * Attempt to convert the Tetra transaction into a [[PolyTransfer]] with proposition type
   * [[ThresholdPropositionCurve25519]].
   * @return if successful, a [[PolyTransfer]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  private def asPolyTransferThreshold: ToDionTxResult[PolyTransfer[ThresholdPropositionCurve25519]] =
    for {
      attestation <- thresholdAttestation
      coinOutputs <- polyOutputs
      tx =
        PolyTransfer(
          getFrom,
          coinOutputs.prepend(getFeeOutput).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  /**
   * Attempt to convert the Tetra transaction into an [[ArbitTransfer]] with proposition type
   * [[PublicKeyPropositionCurve25519]].
   * @return if successful, an [[ArbitTransfer]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  private def asArbitTransferCurve: ToDionTxResult[ArbitTransfer[PublicKeyPropositionCurve25519]] =
    for {
      attestation <- curveAttestation
      coinOutputs <- arbitOutputs
      tx =
        ArbitTransfer(
          getFrom,
          coinOutputs.prepend(getFeeOutput).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  /**
   * Attempt to convert the Tetra transaction into an [[ArbitTransfer]] with proposition type
   * [[PublicKeyPropositionEd25519]].
   * @return if successful, an [[ArbitTransfer]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  private def asArbitTransferEd: ToDionTxResult[ArbitTransfer[PublicKeyPropositionEd25519]] =
    for {
      attestation <- edAttestation
      coinOutputs <- arbitOutputs
      tx =
        ArbitTransfer(
          getFrom,
          coinOutputs.prepend(getFeeOutput).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  /**
   * Attempt to convert the Tetra transaction into an [[ArbitTransfer]] with proposition type
   * [[ThresholdPropositionCurve25519]].
   * @return if successful, an [[ArbitTransfer]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  private def asArbitTransferThreshold: ToDionTxResult[ArbitTransfer[ThresholdPropositionCurve25519]] =
    for {
      attestation <- thresholdAttestation
      coinOutputs <- arbitOutputs
      tx =
        ArbitTransfer(
          getFrom,
          coinOutputs.prepend(getFeeOutput).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  /**
   * Attempt to convert the Tetra transaction into an [[AssetTransfer]] with proposition type
   * [[PublicKeyPropositionCurve25519]].
   * @return if successful, an [[AssetTransfer]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  private def asAssetTransferCurve: ToDionTxResult[AssetTransfer[PublicKeyPropositionCurve25519]] =
    for {
      attestation <- curveAttestation
      coinOutputs <- assetOutputs
      tx =
        AssetTransfer(
          getFrom,
          coinOutputs.prepend(getFeeOutput).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  /**
   * Attempt to convert the Tetra transaction into an [[AssetTransfer]] with proposition type
   * [[PublicKeyPropositionEd25519]].
   * @return if successful, an [[AssetTransfer]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  private def asAssetTransferEd: ToDionTxResult[AssetTransfer[PublicKeyPropositionEd25519]] =
    for {
      attestation <- edAttestation
      coinOutputs <- assetOutputs
      tx =
        AssetTransfer(
          getFrom,
          coinOutputs.prepend(getFeeOutput).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  /**
   * Attempt to convert the Tetra transaction into an [[AssetTransfer]] with proposition type
   * [[ThresholdPropositionCurve25519]].
   * @return if successful, an [[AssetTransfer]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  private def asAssetTransferThreshold: ToDionTxResult[AssetTransfer[ThresholdPropositionCurve25519]] =
    for {
      attestation <- thresholdAttestation
      coinOutputs <- assetOutputs
      tx =
        AssetTransfer(
          getFrom,
          coinOutputs.prepend(getFeeOutput).iterator.toIndexedSeq,
          attestation,
          getFee,
          transaction.timestamp,
          getData,
          transaction.minting
        )
    } yield tx

  /**
   * Attempts to extract a Dion-compatible attestation map from the Tetra transaction.
   * @return if successful, an attestation of type [[PublicKeyPropositionCurve25519]], otherwise a [[ToDionTxFailure]]
   *         representing an error with the extraction
   */
  private def curveAttestation: ToDionTxResult[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]] =
    transaction.inputs.values.toList
      .traverse[ToDionTxResult, (PublicKeyPropositionCurve25519, SignatureCurve25519)] {
        case (prop: Propositions.Knowledge.Curve25519, proof: Proofs.Knowledge.Curve25519) =>
          (
            PublicKeyPropositionCurve25519(PublicKey(prop.key.bytes.data.toArray)),
            SignatureCurve25519(Signature(proof.bytes.data.toArray))
          ).asRight
        case invalid => ToDionTxFailures.InvalidProposition(invalid._1).asLeft
      }
      .map(ListMap(_: _*))

  /**
   * Attempts to extract a Dion-compatible attestation map from the Tetra transaction.
   * @return if successful, an attestation of type [[PublicKeyPropositionEd25519]], otherwise a [[ToDionTxFailure]]
   *         representing an error with the extraction
   */
  private def edAttestation: Either[ToDionTxFailure, ListMap[PublicKeyPropositionEd25519, SignatureEd25519]] =
    transaction.inputs.values.toList
      .traverse[ToDionTxResult, (PublicKeyPropositionEd25519, SignatureEd25519)] {
        case (prop: Propositions.Knowledge.Ed25519, proof: Proofs.Knowledge.Ed25519) =>
          (
            PublicKeyPropositionEd25519(PublicKey(prop.key.bytes.data.toArray)),
            SignatureEd25519(Signature(proof.bytes.data.toArray))
          ).asRight
        case invalid => ToDionTxFailures.InvalidProposition(invalid._1).asLeft
      }
      .map(ListMap(_: _*))

  /**
   * Attempts to extract a Dion-compatible attestation map from the Tetra transaction.
   * @return if successful, an attestation of type [[ThresholdPropositionCurve25519]], otherwise a [[ToDionTxFailure]]
   *         representing an error with the extraction
   */
  private def thresholdAttestation
    : Either[ToDionTxFailure, ListMap[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]] =
    transaction.inputs.values.toList
      .traverse[ToDionTxResult, (ThresholdPropositionCurve25519, ThresholdSignatureCurve25519)] {
        case (prop: Propositions.Compositional.Threshold, proof: Proofs.Compositional.Threshold) =>
          for {
            props <-
              prop.propositions.toList
                .traverse[ToDionTxResult, PublicKeyPropositionCurve25519] {
                  case p: Propositions.Knowledge.Curve25519 =>
                    PublicKeyPropositionCurve25519(PublicKey(p.key.bytes.data.toArray)).asRight
                  case invalid => ToDionTxFailures.InvalidProposition(invalid).asLeft
                }
            proofs <-
              proof.proofs
                .traverse[ToDionTxResult, SignatureCurve25519] {
                  case p: Proofs.Knowledge.Curve25519 =>
                    SignatureCurve25519(Signature(p.bytes.data.toArray)).asRight
                  case invalid => ToDionTxFailures.InvalidProof(invalid).asLeft
                }
            thresholdProp = ThresholdPropositionCurve25519(prop.threshold, SortedSet(props: _*))
            thresholdProof = ThresholdSignatureCurve25519(proofs.toSet)
          } yield thresholdProp -> thresholdProof
        case invalid => ToDionTxFailures.InvalidProposition(invalid._1).asLeft
      }
      .map(ListMap(_: _*))

  private def polyOutputs: ToDionTxResult[NonEmptyChain[(Address, SimpleValue)]] =
    transaction.coinOutputs
      .traverse[ToDionTxResult, (Address, SimpleValue)] {
        case polyOutput: Transaction.PolyOutput =>
          (toAddress(polyOutput.dionAddress), SimpleValue(Int128(polyOutput.value.data))).asRight
        case invalidCoin => ToDionTxFailures.InvalidOutput(invalidCoin).asLeft
      }

  private def arbitOutputs: ToDionTxResult[NonEmptyChain[(Address, SimpleValue)]] =
    transaction.coinOutputs
      .traverse[ToDionTxResult, (Address, SimpleValue)] {
        case arbitOutput: Transaction.ArbitOutput =>
          (toAddress(arbitOutput.dionAddress), SimpleValue(Int128(arbitOutput.value.data))).asRight
        case invalidCoin => ToDionTxFailures.InvalidOutput(invalidCoin).asLeft
      }

  private def assetOutputs: Either[ToDionTxFailure, NonEmptyChain[(Address, TokenValueHolder)]] =
    transaction.coinOutputs
      .traverse[ToDionTxResult, (Address, TokenValueHolder)] {
        case assetOutput: Transaction.AssetOutput =>
          (
            toAddress(assetOutput.dionAddress),
            AssetValue(
              Int128(assetOutput.value.quantity.data),
              AssetCode(
                assetOutput.value.assetCode.version,
                toAddress(assetOutput.value.assetCode.issuer),
                Latin1Data.fromData(assetOutput.value.assetCode.shortName.data.bytes)
              ),
              SecurityRoot(assetOutput.value.securityRoot.toArray),
              assetOutput.value.metadata.map(data => Latin1Data.fromData(data.data.bytes))
            ): TokenValueHolder
          ).asRight
        case invalidCoin => ToDionTxFailures.InvalidOutput(invalidCoin).asLeft
      }

  private def getFrom: IndexedSeq[(Address, Box.Nonce)] =
    transaction.inputs.toIndexedSeq.map(input => toAddress(input._1._1) -> input._1._2)

  private def getFee: Int128 = Int128(transaction.fee.data)

  private def getData: Option[Latin1Data] = transaction.data.map(d => Latin1Data.fromData(d.data.bytes))

  private def getFeeOutput: (Address, SimpleValue) =
    transaction.feeOutput
      /*
        when the fee output is None, use an invalid address and 0 output
        this will not affect the outcome of the transaction signature or codecs
        as this output will be discarded
       */
      .fold(Address(Evidence(Array.empty))(0.toByte) -> SimpleValue(0))(fee =>
        toAddress(fee.dionAddress) -> SimpleValue(Int128(fee.value.data))
      )

  private def toAddress(dionAddress: DionAddress): Address =
    Address(Evidence(dionAddress.typedEvidence.allBytes.toArray))(dionAddress.networkPrefix.value)
}

object TetraTransactionOps {

  sealed trait ToDionTxFailure

  object ToDionTxFailures {
    case class InvalidOutput(invalidOutput: Transaction.CoinOutput) extends ToDionTxFailure
    case class InvalidProposition(propositon: Proposition) extends ToDionTxFailure
    case class InvalidProof(proof: Proof) extends ToDionTxFailure
    case class InvalidTransferType(proposition: Proposition, coinType: Transaction.CoinOutput) extends ToDionTxFailure
    case object EmptyInputs extends ToDionTxFailure
  }

  type ToDionTxResult[T] = Either[ToDionTxFailure, T]

  trait ToTetraTransactionOps {

    implicit def tetraTransactionOpsFromTetraTransaction(transaction: Transaction): TetraTransactionOps =
      new TetraTransactionOps(transaction)
  }

  trait Implicits extends ToTetraTransactionOps

  object implicits extends Implicits
}
