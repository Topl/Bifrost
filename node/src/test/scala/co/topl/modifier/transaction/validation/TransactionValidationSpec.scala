package co.topl.modifier.transaction.validation

import cats.data.NonEmptyChain
import cats.scalatest.{ValidatedMatchers, ValidatedNecMatchers}
import co.topl.attestation.{Address, Proposition, PublicKeyPropositionCurve25519, PublicKeyPropositionEd25519}
import co.topl.consensus.GenesisProvider
import co.topl.modifier.box._
import co.topl.modifier.transaction._
import co.topl.modifier.transaction.validation.implicits._
import co.topl.nodeView.state.State
import co.topl.nodeView.{NodeViewTestHelpers, ValidTransactionGenerators}
import co.topl.utils.GeneratorOps.GeneratorOps
import co.topl.utils.NetworkType.PrivateTestnet
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.{InMemoryKeyRingTestHelper, Int128, NetworkType, TestSettings}
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class TransactionValidationSpec
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with NodeViewTestHelpers
    with InMemoryKeyRingTestHelper
    with ValidTransactionGenerators
    with EitherValues
    with ValidatedMatchers
    with ValidatedNecMatchers {

  type TransferTx = TransferTransaction[TokenValueHolder, _ <: Proposition]

  property("Randomly generated AssetTransfer Tx should be valid") {
    withValidState { boxState =>
      forAll(
        validAssetTransferGen(
          keyRingCurve25519,
          keyRingEd25519,
          propsThresholdCurve25519,
          boxState,
          minting = true
        )
      ) { tx =>
        tx.syntacticValidation should beValid[TransferTx](tx)
      }
    }
  }

  property("AssetTransfer with minting = true should fail if no PolyBoxes are provided") {
    withValidState { boxState =>
      val genCurve25519 =
        validAssetTransferCurve25519Gen(keyRingCurve25519, boxState, fee = 0, minting = true).map { tx =>
          val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = tx.from
            .map { case (address, nonce) =>
              val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
              val box = boxState.getBox(boxId).get
              (address, nonce, box)
            }
            .collect { case (address, nonce, _: AssetBox) =>
              (address, nonce)
            }

          val noPolyRawTx = tx.copy(from = fromWithoutPolys)
          val sender = keyRingCurve25519.addresses.head
          noPolyRawTx.copy(attestation =
            Transaction.updateAttestation(noPolyRawTx)(keyRingCurve25519.generateAttestation(sender))
          )
        }
      forAll(genCurve25519)(tx =>
        tx.semanticValidation(boxState) should haveInvalidC[SemanticValidationFailure](
          SyntacticSemanticValidationFailure(
            NonEmptyChain(NoInputBoxesSpecified)
          )
        )
      )

      val genEd25519 = validAssetTransferEd25519Gen(keyRingEd25519, boxState, fee = 0, minting = true).map { tx =>
        val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = tx.from
          .map { case (address, nonce) =>
            val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
            val box = boxState.getBox(boxId).get
            (address, nonce, box)
          }
          .collect { case (address, nonce, _: AssetBox) =>
            (address, nonce)
          }

        val noPolyRawTx = tx.copy(from = fromWithoutPolys)
        val sender = keyRingEd25519.addresses.head
        noPolyRawTx.copy(attestation =
          Transaction.updateAttestation(noPolyRawTx)(keyRingEd25519.generateAttestation(sender))
        )
      }
      forAll(genEd25519)(tx =>
        tx.semanticValidation(boxState) should haveInvalidC[SemanticValidationFailure](
          SyntacticSemanticValidationFailure(
            NonEmptyChain(NoInputBoxesSpecified)
          )
        )
      )
    }
  }

  property("Attempting to validate a AssetTransfer without valid signature should error") {
    // Create invalid AssetTransfer
    // send tx to state
    forAll(assetTransferGen) { tx =>
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](UnsatisfiedProposition)
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](PropositionEvidenceMismatch)
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](MintingMissingIssuersSignature)
    }
  }

  property("Attempting to validate an AssetTransfer with data of invalid length should error") {
    forAll(stringGen) { data: String =>
      whenever(data.length >= 128) {
        val tx = assetTransferEd25519Gen.sampleFirst()
        val invalidDataTx = tx.copy(data = Some(Latin1Data.unsafe(data)))
        invalidDataTx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](DataTooLong)
      }
    }
  }

  property("Attempting to validate an AssetTransfer with metadata of invalid length should error") {
    forAll(stringGen) { metadata: String =>
      whenever(metadata.length >= 128) {
        val tx = assetTransferEd25519Gen.sampleFirst()
        val assetValue = assetValueEd25519Gen.sampleFirst().copy(metadata = Some(Latin1Data.unsafe(metadata)))
        val invalidDataTx = tx.copy(to = IndexedSeq((assetValue.assetCode.issuer, assetValue)))

        invalidDataTx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](MetadataTooLong)
      }
    }
  }

  property("Randomly generated ArbitTransfer Tx should be valid") {
    withValidState { boxState =>
      forAll(validArbitTransferGen(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, boxState)) { tx =>
        tx.syntacticValidation should beValid[TransferTx](tx)
      }
    }
  }

  property("Attempting to validate a ArbitTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(arbitTransferGen) { tx =>
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](UnsatisfiedProposition)
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](PropositionEvidenceMismatch)
    }
  }

  property("ArbitTransfer should fail if no PolyBoxes are provided") {
    withValidState { boxState =>
      val genCurve25519 =
        validArbitTransferCurve25519Gen(keyRingCurve25519, boxState, fee = 0)
          .map { tx =>
            val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = tx.from
              .map { case (address, nonce) =>
                val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
                val box = boxState.getBox(boxId).get
                (address, nonce, box)
              }
              .collect { case (address, nonce, _: ArbitBox) =>
                (address, nonce)
              }

            signTx(tx.copy(from = fromWithoutPolys))

          }
      forAll(genCurve25519) { tx =>
        tx.semanticValidation(boxState) should haveInvalidC[SemanticValidationFailure](
          InputFeeChangeOutputUnequalNonMinting(
            Int128(Int.MaxValue),
            0,
            0
          )
        )
      }

      val genEd25519 =
        validArbitTransferEd25519Gen(keyRingEd25519, boxState, fee = 0)
          .map { tx =>
            val fromWithoutPolys: IndexedSeq[(Address, Box.Nonce)] = tx.from
              .map { case (address, nonce) =>
                val boxId = BoxId.idFromEviNonce(address.evidence, nonce)
                val box = boxState.getBox(boxId).get
                (address, nonce, box)
              }
              .collect { case (address, nonce, _: ArbitBox) =>
                (address, nonce)
              }

            signTx(tx.copy(from = fromWithoutPolys))

          }
      forAll(genEd25519) { tx =>
        tx.semanticValidation(boxState) should haveInvalidC[SemanticValidationFailure](
          InputFeeChangeOutputUnequalNonMinting(
            Int128(Int.MaxValue),
            0,
            0
          )
        )
      }
    }
  }

  property("Transactions created on a specific network should not be accepted on any other network") {
    withValidState { boxState =>
      val otherNetworks = NetworkType.all.filterNot(_ == PrivateTestnet)
      forAll(
        validAssetTransferGen(
          keyRingCurve25519,
          keyRingEd25519,
          propsThresholdCurve25519,
          boxState,
          minting = true
        )
      ) { tx =>
        otherNetworks.foreach { netType =>
          tx.syntacticValidation(netType.netPrefix) should haveInvalidC[SyntacticValidationFailure](
            MintingMissingIssuersSignature
          )
        }
        tx.syntacticValidation(PrivateTestnet.netPrefix) should beValid[TransferTx](tx)
      }
    }
  }

  property("Randomly generated PolyTransfer Tx should be valid") {
    withValidState { boxState =>
      forAll(validPolyTransferGen(keyRingCurve25519, keyRingEd25519, propsThresholdCurve25519, boxState)) { tx =>
        tx.syntacticValidation should beValid[TransferTx](tx)
      }
    }
  }

  property("Attempting to validate a PolyTransfer without valid signature should error") {
    // Create invalid PolyTransfer
    // send tx to state
    forAll(polyTransferCurve25519Gen) { tx =>
      tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](UnsatisfiedProposition)
    }
  }

  property("Transaction with negative fee should be invalid") {
    withValidState { boxState =>
      forAll(validPolyTransferCurve25519Gen(keyRingCurve25519, boxState).map(tx => signTx(tx.copy(fee = -1)))) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](NegativeFeeFailure)
      }

      forAll(validPolyTransferEd25519Gen(keyRingEd25519, boxState).map(tx => signTx(tx.copy(fee = -1)))) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](NegativeFeeFailure)
      }
    }
  }

  property("Transaction with negative timestamp should be invalid") {
    withValidState { boxState =>
      forAll(
        validPolyTransferCurve25519Gen(keyRingCurve25519, boxState).map(tx => signTx(tx.copy(timestamp = -1)))
      ) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](InvalidTimestamp)
      }

      forAll(validPolyTransferEd25519Gen(keyRingEd25519, boxState).map(tx => signTx(tx.copy(timestamp = -1)))) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](InvalidTimestamp)
      }
    }
  }

  property("Transaction with data length > 128 bytes should be invalid") {
    withValidState { boxState =>
      val data = Random.alphanumeric.take(512).mkString

      forAll(
        validPolyTransferCurve25519Gen(keyRingCurve25519, boxState).map(tx =>
          signTx(tx.copy(data = Some(Latin1Data.unsafe(data))))
        )
      ) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](DataTooLong)
      }

      forAll(
        validPolyTransferEd25519Gen(keyRingEd25519, boxState).map(tx =>
          signTx(tx.copy(data = Some(Latin1Data.unsafe(data))))
        )
      ) { tx =>
        tx.syntacticValidation should haveInvalidC[SyntacticValidationFailure](DataTooLong)
      }
    }
  }

  private def signTx(
    tx: PolyTransfer[PublicKeyPropositionCurve25519]
  ): PolyTransfer[PublicKeyPropositionCurve25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingCurve25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def signTx(tx: => PolyTransfer[PublicKeyPropositionEd25519]): PolyTransfer[PublicKeyPropositionEd25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingEd25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def signTx(
    tx: ArbitTransfer[PublicKeyPropositionCurve25519]
  ): ArbitTransfer[PublicKeyPropositionCurve25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingCurve25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def signTx(
    tx: => ArbitTransfer[PublicKeyPropositionEd25519]
  ): ArbitTransfer[PublicKeyPropositionEd25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingEd25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def signTx(
    tx: AssetTransfer[PublicKeyPropositionCurve25519]
  ): AssetTransfer[PublicKeyPropositionCurve25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingCurve25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def signTx(
    tx: => AssetTransfer[PublicKeyPropositionEd25519]
  ): AssetTransfer[PublicKeyPropositionEd25519] =
    tx.copy(attestation =
      Transaction.updateAttestation(tx)(keyRingEd25519.generateAttestation(tx.from.map(_._1).toSet))
    )

  private def withValidState(test: State => Unit): Unit =
    test(
      generateState(
        GenesisProvider
          .construct(
            keyRingCurve25519.addresses ++ keyRingEd25519.addresses ++ propsThresholdCurve25519.map(_.address),
            Int.MaxValue,
            Long.MaxValue,
            0
          )
          .block
      ).state
    )
}
