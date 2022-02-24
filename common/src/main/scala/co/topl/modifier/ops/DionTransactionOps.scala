package co.topl.modifier.ops

import cats.data.NonEmptyChain
import co.topl.attestation.EvidenceProducer.Syntax._
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
import co.topl.models.utility.Sized
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.modifier.box.{AssetValue, Box => DionBox, PolyBox, SimpleValue, TokenValueHolder}
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction => DionTransaction}
import co.topl.utils.NetworkType.{NetworkPrefix => DionNetworkPrefix}
import scodec.bits.ByteVector

import scala.collection.immutable.{ListMap, ListSet}
import scala.language.implicitConversions

class DionTransactionOps[P <: DionProposition](val transaction: DionTransaction[_, P]) extends AnyVal {

  def upgrade(implicit evidenceProducer: EvidenceProducer[P]): Transaction =
    transaction match {
      case transfer: ArbitTransfer[P] =>
        Transaction(
          mergeInputsWithAttestation(transfer.from, transfer.attestation),
          getFeeOutput(transfer.to.head),
          getCoinOutputs(transfer.to.tail, DionTransactionOps.TransferTypes.Arbits),
          Sized.maxUnsafe(transfer.fee.toInt),
          transfer.timestamp,
          transfer.data.map(x => Sized.maxUnsafe(Latin1Data.fromData(x.value))),
          transfer.minting
        )
      case transfer: AssetTransfer[P] =>
        Transaction(
          mergeInputsWithAttestation(transfer.from, transfer.attestation),
          getFeeOutput(transfer.to.head),
          getCoinOutputs(transfer.to.tail, DionTransactionOps.TransferTypes.Assets),
          Sized.maxUnsafe(transfer.fee.toInt),
          transfer.timestamp,
          transfer.data.map(x => Sized.maxUnsafe(Latin1Data.fromData(x.value))),
          transfer.minting
        )
      case transfer: PolyTransfer[P] =>
        Transaction(
          mergeInputsWithAttestation(transfer.from, transfer.attestation),
          getFeeOutput(transfer.to.head),
          getCoinOutputs(transfer.to.tail, DionTransactionOps.TransferTypes.Polys),
          Sized.maxUnsafe(transfer.fee.toInt),
          transfer.timestamp,
          transfer.data.map(x => Sized.maxUnsafe(Latin1Data.fromData(x.value))),
          transfer.minting
        )
    }

  private def mergeInputsWithAttestation[D <: DionProposition: EvidenceProducer](
    inputs:      IndexedSeq[(Address, DionBox.Nonce)],
    attestation: ListMap[D, DionProof[D]]
  ): ListMap[BoxReference, (Proposition, Proof)] =
    attestation.foldLeft(ListMap.empty[BoxReference, (Proposition, Proof)]) { case (result, pair) =>
      val evidence = pair._1.generateEvidence
      val propProofPair = pair match {
        case (prop: PublicKeyPropositionCurve25519, proof: SignatureCurve25519) =>
          (
            Propositions.Knowledge.Curve25519(
              VerificationKeys.Curve25519(
                Sized.strictUnsafe(ByteVector(prop.pubKeyBytes.value))
              )
            ),
            Proofs.Knowledge.Curve25519(Sized.strictUnsafe(ByteVector(proof.sigBytes.value)))
          )
        case (prop: PublicKeyPropositionEd25519, proof: SignatureEd25519) =>
          (
            Propositions.Knowledge.Ed25519(
              VerificationKeys.Ed25519(
                Sized.strictUnsafe(ByteVector(prop.pubKeyBytes.value))
              )
            ),
            Proofs.Knowledge.Curve25519(Sized.strictUnsafe(ByteVector(proof.sigBytes.value)))
          )
        case (prop: ThresholdPropositionCurve25519, proof: ThresholdSignatureCurve25519) =>
          (
            Propositions.Compositional.Threshold(
              prop.threshold,
              ListSet
                .from(prop.pubKeyProps)
                .map(p =>
                  Propositions.Knowledge
                    .Curve25519(VerificationKeys.Curve25519(Sized.strictUnsafe(ByteVector(p.pubKeyBytes.value))))
                )
            ),
            Proofs.Compositional.Threshold(
              List
                .from(proof.signatures)
                .map(p => Proofs.Knowledge.Curve25519(Sized.strictUnsafe(ByteVector(p.sigBytes.value))))
            )
          )
      }
      result ++ inputs.filter(_._1.evidence == evidence).map(input => (input._1.upgrade, input._2) -> propProofPair)
    }

  private def getFeeOutput(
    from: (Address, TokenValueHolder)
  ): Option[Transaction.PolyOutput] =
    Option.when(from._2.quantity > 0)(
      Transaction.PolyOutput(
        DionAddress(NetworkPrefix(from._1.networkPrefix), from._1.evidence.upgrade),
        Sized.maxUnsafe(from._2.quantity.toLong)
      )
    )

  private def getCoinOutputs(
    from:         IndexedSeq[(Address, TokenValueHolder)],
    transferType: DionTransactionOps.TransferType
  ): NonEmptyChain[Transaction.CoinOutput] =
    for {
      coinPair <- NonEmptyChain.fromSeq(from).get
      address = coinPair._1.upgrade
      coin =
        (transferType, coinPair._2) match {
          case (DionTransactionOps.TransferTypes.Polys, SimpleValue(quantity)) =>
            Transaction.PolyOutput(address, Sized.maxUnsafe(quantity.toInt))
          case (DionTransactionOps.TransferTypes.Arbits, SimpleValue(quantity)) =>
            Transaction.ArbitOutput(address, Sized.maxUnsafe(quantity.toInt))
          case (DionTransactionOps.TransferTypes.Assets, AssetValue(quantity, assetCode, securityRoot, metadata)) =>
            Transaction.AssetOutput(
              address,
              Box.Values.Asset(
                Sized.maxUnsafe(quantity.toInt),
                Box.Values.Asset
                  .Code(
                    assetCode.version,
                    assetCode.issuer.upgrade,
                    Sized.maxUnsafe(Latin1Data.fromData(assetCode.shortName.value))
                  ),
                ByteVector(securityRoot.root),
                metadata.map(m => Sized.maxUnsafe(Latin1Data.fromData(m.value)))
              )
            )
        }
    } yield coin
}

object DionTransactionOps {

  sealed trait TransferType

  object TransferTypes {
    case object Assets extends TransferType
    case object Arbits extends TransferType
    case object Polys extends TransferType
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
