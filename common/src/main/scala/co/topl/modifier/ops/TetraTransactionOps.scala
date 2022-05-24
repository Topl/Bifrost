package co.topl.modifier.ops

import cats.data.Chain
import cats.implicits._
import co.topl.attestation._
import co.topl.crypto.{PublicKey, Signature}
import co.topl.models.{Box => TetraBox, FullAddress, Proof, Proofs, Proposition, Propositions, Transaction}
import co.topl.modifier.box.{AssetCode, AssetValue, Box, SecurityRoot, SimpleValue, TokenValueHolder}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction => DionTransaction}
import co.topl.typeclasses.implicits.TransactionSupport
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

import scala.collection.SortedSet
import scala.collection.immutable.ListMap
import scala.language.implicitConversions

class TetraTransactionOps(private val transaction: Transaction) extends AnyVal {
  import TetraTransactionOps._

  def minting: Boolean = transaction.outputs.exists(_.minting)

  /**
   * Attempts to convert a Tetra [[Transaction]] into an equivalent [[DionTransaction.TX]] value.
   * @return if successful, a [[DionTransaction.TX]], otherwise a [[ToDionTxFailure]] representing an error with the
   *         conversion
   */
  def toDionTx: ToDionTxResult[DionTransaction.TX] =
    for {
      // all propositions in the Dion transfer must be the same as the first one
      expectedProposition <- transaction.inputs.headOption.toRight(ToDionTxFailures.EmptyInputs).map(_.proposition)
      headOutput          <- transaction.outputs.headOption.toRight(ToDionTxFailures.EmptyOutputs)
      tx                  <-
        // use the first proposition type and first coin output type to derive what the Dion transfer type will be
        (expectedProposition, headOutput.value) match {
          case (_: Propositions.Knowledge.Curve25519, _: TetraBox.Values.Poly)     => asPolyTransferCurve
          case (_: Propositions.Knowledge.Curve25519, _: TetraBox.Values.Arbit)    => asArbitTransferCurve
          case (_: Propositions.Knowledge.Curve25519, _: TetraBox.Values.Asset)    => asAssetTransferCurve
          case (_: Propositions.Knowledge.Ed25519, _: TetraBox.Values.Poly)        => asPolyTransferEd
          case (_: Propositions.Knowledge.Ed25519, _: TetraBox.Values.Arbit)       => asArbitTransferEd
          case (_: Propositions.Knowledge.Ed25519, _: TetraBox.Values.Asset)       => asAssetTransferEd
          case (_: Propositions.Compositional.Threshold, _: TetraBox.Values.Poly)  => asPolyTransferThreshold
          case (_: Propositions.Compositional.Threshold, _: TetraBox.Values.Arbit) => asArbitTransferThreshold
          case (_: Propositions.Compositional.Threshold, _: TetraBox.Values.Asset) => asAssetTransferThreshold
          case (prop, output) =>
            ToDionTxFailures.InvalidTransferType(prop, headOutput).asLeft[DionTransaction.TX]
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
          transaction.chronology.creation,
          getData,
          minting
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
          transaction.chronology.creation,
          getData,
          minting
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
          transaction.chronology.creation,
          getData,
          minting
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
          transaction.chronology.creation,
          getData,
          minting
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
          transaction.chronology.creation,
          getData,
          minting
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
          transaction.chronology.creation,
          getData,
          minting
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
          transaction.chronology.creation,
          getData,
          minting
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
          transaction.chronology.creation,
          getData,
          minting
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
          transaction.chronology.creation,
          getData,
          minting
        )
    } yield tx

  /**
   * Attempts to extract a Dion-compatible attestation map from the Tetra transaction.
   * @return if successful, an attestation of type [[PublicKeyPropositionCurve25519]], otherwise a [[ToDionTxFailure]]
   *         representing an error with the extraction
   */
  private def curveAttestation: ToDionTxResult[ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519]] =
    transaction.inputs
      .traverse[ToDionTxResult, (PublicKeyPropositionCurve25519, SignatureCurve25519)] {
        case Transaction.Input(_, _, prop: Propositions.Knowledge.Curve25519, proof: Proofs.Knowledge.Curve25519, _) =>
          (
            PublicKeyPropositionCurve25519(PublicKey(prop.key.bytes.data.toArray)),
            SignatureCurve25519(Signature(proof.bytes.data.toArray))
          ).asRight
        case invalid => ToDionTxFailures.InvalidProposition(invalid.proposition).asLeft
      }
      .map(ListMap.empty[PublicKeyPropositionCurve25519, SignatureCurve25519] ++ _.toIterable)

  /**
   * Attempts to extract a Dion-compatible attestation map from the Tetra transaction.
   * @return if successful, an attestation of type [[PublicKeyPropositionEd25519]], otherwise a [[ToDionTxFailure]]
   *         representing an error with the extraction
   */
  private def edAttestation: Either[ToDionTxFailure, ListMap[PublicKeyPropositionEd25519, SignatureEd25519]] =
    transaction.inputs
      .traverse[ToDionTxResult, (PublicKeyPropositionEd25519, SignatureEd25519)] {
        case Transaction.Input(_, _, prop: Propositions.Knowledge.Ed25519, proof: Proofs.Knowledge.Ed25519, _) =>
          (
            PublicKeyPropositionEd25519(PublicKey(prop.key.bytes.data.toArray)),
            SignatureEd25519(Signature(proof.bytes.data.toArray))
          ).asRight
        case invalid => ToDionTxFailures.InvalidProposition(invalid.proposition).asLeft
      }
      .map(ListMap.empty[PublicKeyPropositionEd25519, SignatureEd25519] ++ _.toIterable)

  /**
   * Attempts to extract a Dion-compatible attestation map from the Tetra transaction.
   * @return if successful, an attestation of type [[ThresholdPropositionCurve25519]], otherwise a [[ToDionTxFailure]]
   *         representing an error with the extraction
   */
  private def thresholdAttestation
    : Either[ToDionTxFailure, ListMap[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]] =
    transaction.inputs
      .traverse[ToDionTxResult, (ThresholdPropositionCurve25519, ThresholdSignatureCurve25519)] {
        case Transaction.Input(
              _,
              _,
              prop: Propositions.Compositional.Threshold,
              proof: Proofs.Compositional.Threshold,
              _
            ) =>
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
        case invalid => ToDionTxFailures.InvalidProposition(invalid.proposition).asLeft
      }
      .map(ListMap.empty[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519] ++ _.toIterable)

  private def polyOutputs: ToDionTxResult[Chain[(Address, SimpleValue)]] =
    transaction.outputs
      .traverse[ToDionTxResult, (Address, SimpleValue)] {
        case Transaction.Output(address, poly: TetraBox.Values.Poly, _) =>
          (toAddress(address), SimpleValue(Int128(poly.quantity.data))).asRight
        case invalidCoin => ToDionTxFailures.InvalidOutput(invalidCoin).asLeft
      }

  private def arbitOutputs: ToDionTxResult[Chain[(Address, SimpleValue)]] =
    transaction.outputs
      .traverse[ToDionTxResult, (Address, SimpleValue)] {
        case Transaction.Output(address, arbit: TetraBox.Values.Arbit, _) =>
          (toAddress(address), SimpleValue(Int128(arbit.quantity.data))).asRight
        case invalidCoin => ToDionTxFailures.InvalidOutput(invalidCoin).asLeft
      }

  private def assetOutputs: Either[ToDionTxFailure, Chain[(Address, TokenValueHolder)]] =
    transaction.outputs
      .traverse[ToDionTxResult, (Address, TokenValueHolder)] {
        case Transaction.Output(address, asset: TetraBox.Values.Asset, _) =>
          (
            toAddress(address),
            AssetValue(
              Int128(asset.quantity.data),
              AssetCode(
                asset.assetCode.version,
                Address(Evidence(asset.assetCode.issuer.typedEvidence.allBytes.toArray))(???),
                Latin1Data.fromData(asset.assetCode.shortName.data.bytes)
              ),
              SecurityRoot(asset.securityRoot.data.toArray),
              asset.metadata.map(data => Latin1Data.fromData(data.data.bytes))
            ): TokenValueHolder
          ).asRight
        case invalidCoin => ToDionTxFailures.InvalidOutput(invalidCoin).asLeft
      }

  // TODO: Unable to determine the input Box Nonce(s) from a Tetra Transaction
  private def getFrom: IndexedSeq[(Address, Box.Nonce)] =
    ???
//    transaction.inputs.toNonEmptyVector.toVector.map(input => toAddress(input.proposition.dionAddress) -> input._1._2)

  private def getFee: Int128 =
    transaction.unclaimedInputValues
      .collectFirst { case TetraBox.Values.Poly(value) =>
        Int128(value.data)
      }
      .getOrElse(Int128(0))

  private def getData: Option[Latin1Data] = transaction.data.map(d => Latin1Data.fromData(d.data.bytes))

  // TODO
  private def getFeeOutput: (Address, SimpleValue) = ???
//      /*
//        when the fee output is None, use an invalid address and 0 output
//        this will not affect the outcome of the transaction signature or codecs
//        as this output will be discarded
//       */
//      .fold(Address(Evidence(Array.empty))(0.toByte) -> SimpleValue(0))(fee =>
//        toAddress(fee.dionAddress) -> SimpleValue(Int128(fee.value.data))
//      )

  private def toAddress(fullAddress: FullAddress): Address =
    Address(Evidence(fullAddress.spendingAddress.typedEvidence.allBytes.toArray))(fullAddress.networkPrefix.value)
}

object TetraTransactionOps {

  sealed trait ToDionTxFailure

  object ToDionTxFailures {
    case class InvalidOutput(invalidOutput: Transaction.Output) extends ToDionTxFailure
    case class InvalidProposition(propositon: Proposition) extends ToDionTxFailure
    case class InvalidProof(proof: Proof) extends ToDionTxFailure
    case class InvalidTransferType(proposition: Proposition, coinType: Transaction.Output) extends ToDionTxFailure
    case object EmptyInputs extends ToDionTxFailure
    case object EmptyOutputs extends ToDionTxFailure
  }

  type ToDionTxResult[T] = Either[ToDionTxFailure, T]

  trait ToTetraTransactionOps {

    implicit def tetraTransactionOpsFromTetraTransaction(transaction: Transaction): TetraTransactionOps =
      new TetraTransactionOps(transaction)
  }

  trait Implicits extends ToTetraTransactionOps

  object implicits extends Implicits
}
