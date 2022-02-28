package co.topl.modifier.transaction.ops

import co.topl.attestation.{Address, PublicKeyPropositionEd25519, SignatureEd25519}
import co.topl.crypto.PublicKey
import co.topl.models.Transaction
import co.topl.modifier.box.Box
import co.topl.modifier.implicits._
import co.topl.modifier.transaction.PolyTransfer
import co.topl.utils.CommonGenerators
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.encode.Base16
import org.scalacheck.Gen
import org.scalatest.{EitherValues, OptionValues}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ListMap

class DionTransactionOpsSpec
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with CommonGenerators
    with OptionValues
    with ScalaCheckDrivenPropertyChecks {

  private def inputGen(
    fromPublicKeys:         List[PublicKey]
  )(implicit networkPrefix: NetworkPrefix): Gen[List[(Address, Box.Nonce)]] =
    Gen
      .listOfN(fromPublicKeys.length, Gen.long)
      .map(longs => fromPublicKeys.map(pk => Address.from(PublicKeyPropositionEd25519(pk))).zip(longs))

  private def attestationGen(
    fromPublicKeys: List[PublicKey]
  ): Gen[ListMap[PublicKeyPropositionEd25519, SignatureEd25519]] =
    Gen
      .listOfN(fromPublicKeys.length, signatureEd25519Gen)
      .map(signatures => fromPublicKeys.map(pk => PublicKeyPropositionEd25519(pk)).zip(signatures))
      .map(list => ListMap(list: _*))

  def txGen(
    fromPublicKeys:         List[PublicKey]
  )(implicit networkPrefix: NetworkPrefix): Gen[PolyTransfer[PublicKeyPropositionEd25519]] =
    polyTransferEd25519Gen.flatMap(transfer =>
      Gen
        .zip(
          inputGen(fromPublicKeys),
          attestationGen(fromPublicKeys),
          toSeqGen.filter(_.length > 1)
        )
        .map(genValues =>
          transfer.copy(from = genValues._1.toIndexedSeq, attestation = genValues._2, to = genValues._3)
        )
    )

  private val txGen: Gen[PolyTransfer[PublicKeyPropositionEd25519]] =
    Gen
      .zip(Gen.nonEmptyListOf(publicKeyPropositionEd25519Gen.map(_._2.pubKeyBytes)), Gen.posNum[Int].map(_.toByte))
      .flatMap(pair => txGen(pair._1)(pair._2))

  describe("DionTransactionOps") {
    describe("toTetraTx") {
      it("should convert a Dion TX to a Tetra TX with the same fee value") {
        forAll(txGen) { tx =>
          val tetraTx = tx.toTetraTx

          tetraTx.value.fee.data shouldBe BigInt(tx.fee.toByteArray)
        }
      }

      it("should convert a Dion TX to a Tetra TX with the same timestamp") {
        forAll(txGen) { tx =>
          val tetraTx = tx.toTetraTx

          tetraTx.value.timestamp shouldBe tx.timestamp
        }
      }

      it("should convert a Dion TX to a Tetra TX with the same data") {
        forAll(txGen) { tx =>
          val expectedData = tx.data.map(_.value).getOrElse(Array.empty)

          val tetraTx = tx.toTetraTx

          val data = tetraTx.value.data.map(_.data.bytes).getOrElse(Array.empty)

          data shouldBe expectedData
        }
      }

      it("should convert a Dion TX to a Tetra TX with the same minting value") {
        forAll(txGen) { tx =>
          val tetraTx = tx.toTetraTx

          tetraTx.value.minting shouldBe tx.minting
        }
      }

      it("should convert a Dion TX to a Tetra TX with the same ordering of inputs") {
        forAll(txGen) { tx =>
          val expectedInput = tx.from.toList.map { case (address, nonce) =>
            (Base16.encode(address.networkPrefix +: address.evidence.evBytes), nonce)
          }

          val tetraTx = tx.toTetraTx

          val input = tetraTx.value.inputs.keys.map { case (address, nonce) =>
            (address.allBytes.toBase16, nonce)
          }

          input shouldBe expectedInput
        }
      }

      it("should convert a Dion TX to a Tetra TX with the same ordering of outputs") {
        forAll(txGen) { tx =>
          val expectedOutputs = tx.to.tail.toList.map { case (address, value) =>
            (Base16.encode(address.networkPrefix +: address.evidence.evBytes), BigInt(value.quantity.toByteArray))
          }

          val tetraTx = tx.toTetraTx

          val ouputs = tetraTx.value.coinOutputs.iterator.toList.map {
            case Transaction.PolyOutput(dionAddress, value) =>
              (dionAddress.allBytes.toBase16, value.data)
          }

          ouputs shouldBe expectedOutputs
        }
      }

      it("should convert a Dion TX to a Tetra TX with no fee output when first output fee is 0") {
        forAll(txGen) { tx =>
          val firstOutput = tx.to.head
          val outputWith0Fee = firstOutput._1 -> firstOutput._2.copy(quantity = 0)

          val tetraTx = tx.copy(to = tx.to.tail.prepended(outputWith0Fee)).toTetraTx

          tetraTx.value.feeOutput shouldBe None
        }
      }

      it("should convert a Dion TX to a Tetra TX with no fee output when first output fee is some non-zero") {
        forAll(txGen, Gen.posNum[BigInt]) { (tx, fee) =>
          val firstOutput = tx.to.head
          val outputWith0Fee = firstOutput._1 -> firstOutput._2.copy(quantity = fee)

          val tetraTx = tx.copy(to = tx.to.tail.prepended(outputWith0Fee)).toTetraTx

          tetraTx.value.feeOutput.value.value.data shouldBe fee
        }
      }
    }
  }
}
