package co.topl.modifier.transaction.ops

import cats.data.NonEmptyChain
import co.topl.attestation.{PublicKeyPropositionEd25519, SignatureEd25519}
import co.topl.models._
import co.topl.modifier.implicits._
import co.topl.modifier.ops.TetraTransactionOps
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.utils.CommonGenerators
import co.topl.utils.encode.Base16
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ListMap

class TetraTransactionOpsSpec
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with CommonGenerators
    with ScalaCheckDrivenPropertyChecks {

  object ModelGen extends ModelGenerators

  private def txWithOutputs(outputs: NonEmptyChain[Transaction.CoinOutput]): Gen[Transaction] =
    ModelGen.arbitraryTransaction.arbitrary
      .map(tx => tx.copy(coinOutputs = outputs))

  private val arbitTxGen: Gen[Transaction] =
    Gen
      .nonEmptyListOf(ModelGen.arbitraryArbitOutput.arbitrary)
      .map(arbits => arbits.map(x => x: Transaction.CoinOutput))
      .map(NonEmptyChain.fromSeq)
      .map(_.get)
      .flatMap(outputs => txWithOutputs(outputs))

  private val polyTxGen: Gen[Transaction] =
    Gen
      .nonEmptyListOf(ModelGen.arbitraryPolyOutput.arbitrary)
      .map(arbits => arbits.map(x => x: Transaction.CoinOutput))
      .map(NonEmptyChain.fromSeq)
      .map(_.get)
      .flatMap(outputs => txWithOutputs(outputs))

  private val assetTxGen: Gen[Transaction] =
    Gen
      .nonEmptyListOf(
        ModelGen.arbitraryAssetOutput.arbitrary
          .map(asset =>
            asset.copy(value =
              Box.Values.Asset(
                asset.value.quantity,
                assetCode = asset.value.assetCode.copy(version = 1),
                securityRoot = asset.value.securityRoot,
                metadata = asset.value.metadata
              )
            )
          )
      )
      .map(assets => assets.map(x => x: Transaction.CoinOutput))
      .map(NonEmptyChain.fromSeq)
      .map(_.get)
      .flatMap(outputs => txWithOutputs(outputs))

  describe("TetraTransactionOps") {
    describe("toDionTx") {
      it("should convert a Tetra TX with Arbit outputs to an Arbit Transfer") {
        forAll(arbitTxGen) { tx =>
          val dionTransfer = tx.toDionTx

          dionTransfer.value.isInstanceOf[ArbitTransfer[_]] shouldBe true
        }
      }

      it("should convert a Tetra TX with Poly outputs to a Poly Transfer") {
        forAll(polyTxGen) { tx =>
          val dionTransfer = tx.toDionTx

          dionTransfer.value.isInstanceOf[PolyTransfer[_]] shouldBe true
        }
      }

      it("should convert a Tetra TX with Asset outputs to an Asset Transfer") {
        forAll(assetTxGen) { tx =>
          val dionTransfer = tx.toDionTx

          dionTransfer.value.isInstanceOf[AssetTransfer[_]] shouldBe true
        }
      }

      it("should convert a Tetra TX into a Dion TX with the same fee value") {
        forAll(polyTxGen) { tx =>
          val dionTransfer = tx.toDionTx

          BigInt(dionTransfer.value.fee.toByteArray) shouldBe tx.fee.data
        }
      }

      it("should convert a Tetra TX into a Dion TX with the first output being the fee") {
        forAll(polyTxGen) { tx =>
          val expectedFeeAddressBytes = tx.feeOutput.map(_.dionAddress.allBytes.toArray).getOrElse(Array(0.toByte))
          val expectedFeeValue = tx.feeOutput.map(_.value.data).getOrElse(0)

          val dionTransfer = tx.toDionTx

          val feeOutput = dionTransfer.value.asInstanceOf[PolyTransfer[_]].to.head
          val feeAddressBytes = feeOutput._1.networkPrefix +: feeOutput._1.evidence.evBytes
          val feeValue = BigInt(feeOutput._2.quantity.toByteArray)

          feeAddressBytes shouldBe expectedFeeAddressBytes
          feeValue shouldBe expectedFeeValue
        }
      }

      it("should convert a Tetra TX into a Dion TX with the same ordering of inputs") {
        forAll(polyTxGen) { tx =>
          val expectedInputs = tx.inputs.toIndexedSeq.map { case ((address, nonce), _) =>
            // encode array as Base-16 string to ensure a valid equality check
            Base16.encode(address.allBytes.toArray) -> nonce
          }

          val dionTransfer = tx.toDionTx

          val polyTransfer = dionTransfer.value.asInstanceOf[PolyTransfer[_]]

          val inputs = polyTransfer.from.map { case (address, nonce) =>
            Base16.encode(address.networkPrefix +: address.evidence.evBytes) -> nonce
          }

          inputs shouldBe expectedInputs
        }
      }

      it("should convert a Tetra TX into a Dion TX with the same ordering of outputs") {
        forAll(polyTxGen) { tx =>
          val expectedOutputs =
            tx.coinOutputs
              .map(output => output.asInstanceOf[Transaction.PolyOutput])
              // encode output address as a Base-16 string to ensure a valid equality check
              .map(polyOutput => Base16.encode(polyOutput.dionAddress.allBytes.toArray) -> polyOutput.value.data)
              .iterator
              .toList

          val dionTransfer = tx.toDionTx

          val polyTransfer = dionTransfer.value.asInstanceOf[PolyTransfer[_]]

          val outputs =
            polyTransfer.to.tail.map { case (address, value) =>
              Base16.encode(address.networkPrefix +: address.evidence.evBytes) -> BigInt(value.quantity.toByteArray)
            }.toList

          outputs shouldBe expectedOutputs
        }
      }

      it("should convert a Tetra TX into a Dion TX with the same attestation ordering") {
        forAll(polyTxGen) { tx =>
          val expectedAttestation = tx.inputs.values.flatMap {
            case (prop: Propositions.Knowledge.Ed25519, proof: Proofs.Knowledge.Ed25519) =>
              prop.key.bytes.data.toBase16 -> proof.bytes.data.toBase16
            case _ => throw new IllegalStateException("Unexpected attestation type")
          }.toList

          val dionTransfer = tx.toDionTx

          val polyTransfer = dionTransfer.value.asInstanceOf[PolyTransfer[PublicKeyPropositionEd25519]]

          val attestation = polyTransfer.attestation.toList.map {
            case (prop: PublicKeyPropositionEd25519, proof: SignatureEd25519) =>
              Base16.encode(prop.pubKeyBytes.value) -> Base16.encode(proof.sigBytes.value)
          }

          attestation shouldBe expectedAttestation
        }
      }

      it("should convert a Tetra TX into a Dion TX with the same timestamp") {
        forAll(polyTxGen) { tx =>
          val dionTransfer = tx.toDionTx

          val polyTransfer = dionTransfer.value.asInstanceOf[PolyTransfer[_]]

          polyTransfer.timestamp shouldBe tx.timestamp
        }
      }

      it("should convert a Tetra TX into a Dion TX with the same data") {
        forAll(polyTxGen) { tx =>
          val expectedData = tx.data.map(data => data.data.bytes).getOrElse(Array.empty)

          val dionTransfer = tx.toDionTx

          val polyTransfer = dionTransfer.value.asInstanceOf[PolyTransfer[_]]

          val data = polyTransfer.data.map(_.value).getOrElse(Array.empty)

          data shouldBe expectedData
        }
      }

      it("should convert a Tetra TX into a Dion TX with the same minting value") {
        forAll(polyTxGen) { tx =>
          val dionTransfer = tx.toDionTx

          val polyTransfer = dionTransfer.value.asInstanceOf[PolyTransfer[_]]

          polyTransfer.minting shouldBe tx.minting
        }
      }

      it("should fail to convert a Tetra TX into a Dion TX if first output is Poly and contains Arbit output") {
        forAll(polyTxGen, ModelGen.arbitraryDionAddress.arbitrary, ModelGen.arbitraryInt128.arbitrary) {
          (tx, address, value) =>
            val txWithArbitOutputs = tx.copy(coinOutputs = tx.coinOutputs :+ Transaction.ArbitOutput(address, value))

            val result = txWithArbitOutputs.toDionTx

            result.left.value shouldBe a[TetraTransactionOps.ToDionTxFailures.InvalidOutput]
            val invalidOutput = result.left.value.asInstanceOf[TetraTransactionOps.ToDionTxFailures.InvalidOutput]
            invalidOutput.invalidOutput shouldBe a[Transaction.ArbitOutput]
        }
      }

      it("should fail to convert a Tetra TX into a Dion TX if first output is Arbit and contains Poly output") {
        forAll(arbitTxGen, ModelGen.arbitraryDionAddress.arbitrary, ModelGen.arbitraryInt128.arbitrary) {
          (tx, address, value) =>
            val txWithArbitOutputs = tx.copy(coinOutputs = tx.coinOutputs :+ Transaction.PolyOutput(address, value))

            val result = txWithArbitOutputs.toDionTx

            result.left.value shouldBe a[TetraTransactionOps.ToDionTxFailures.InvalidOutput]
            val invalidOutput = result.left.value.asInstanceOf[TetraTransactionOps.ToDionTxFailures.InvalidOutput]
            invalidOutput.invalidOutput shouldBe a[Transaction.PolyOutput]
        }
      }
    }
  }
}
