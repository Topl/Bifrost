package co.topl.modifier.transaction.builder.tetra

import cats.implicits._
import cats.data.{Chain, NonEmptyChain, State}
import co.topl.attestation.Address
import co.topl.models.{ModelGenerators, Transaction}
import co.topl.modifier.transaction.builder.{BoxCache, BoxSelectionAlgorithms}
import co.topl.modifier.transaction.builder.BoxCache.BoxSet
import co.topl.utils.CommonGenerators
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.attestation.implicits._
import co.topl.modifier.implicits._

class TransferBuilderImplSpec
    extends AnyFunSpec
    with ScalaCheckDrivenPropertyChecks
    with CommonGenerators
    with EitherValues
    with Matchers {

  object ModelGen extends ModelGenerators

  type InMemoryBoxState[T] = State[BoxSet, T]

  val inMemoryBoxCache: BoxCache[InMemoryBoxState] =
    (addresses: List[Address]) =>
      State.inspect(boxSet =>
        BoxSet(
          boxSet.arbits.filter(box => addresses.contains(box._1)),
          boxSet.polys.filter(box => addresses.contains(box._1)),
          boxSet.assets.filter(box => addresses.contains(box._1))
        )
      )

  val boxSetGen: Gen[BoxSet] =
    Gen
      .zip(Gen.listOf(arbitBoxGen), Gen.listOf(polyBoxGen), Gen.listOf(assetBoxGen))
      .flatMap(boxes =>
        Gen
          .zip(
            Gen.listOfN(boxes._1.length, addressGen),
            Gen.listOfN(boxes._2.length, addressGen),
            Gen.listOfN(boxes._3.length, addressGen)
          )
          .map(addresses =>
            BoxSet(
              addresses._1.zip(boxes._1),
              addresses._2.zip(boxes._2),
              addresses._3.zip(boxes._3)
            )
          )
      )

  val polyOrArbitGen: Gen[Transaction.CoinOutput] =
    Gen.oneOf[Transaction.CoinOutput](ModelGen.arbitraryPolyOutput.arbitrary, ModelGen.arbitraryArbitOutput.arbitrary)

  val polyAndArbitOutputs: Gen[NonEmptyChain[Transaction.CoinOutput]] =
    Gen
      .zip(polyOrArbitGen, Gen.listOf(polyOrArbitGen))
      .map(outputs => NonEmptyChain.one(outputs._1).appendChain(Chain.fromSeq(outputs._2)))

  describe("TransferBuilderImpl") {

    val underTest: TransferBuilder[InMemoryBoxState] = TransferBuilder(inMemoryBoxCache)

    describe("build") {
      it("should return a transaction with all box references in state when using all strategy") {
        forAll(
          boxSetGen,
          Gen.option(ModelGen.arbitraryPolyOutput.arbitrary),
          polyAndArbitOutputs,
          ModelGen.arbitraryInt128.arbitrary,
          Gen.long,
          Gen.oneOf(true, false)
        ) { (boxSet, feeOutput, outputs, feeValue, timestamp, minting) =>
          // only get poly and arbit box references
          val boxReferences = BoxSet.toBoxReferences(boxSet.copy(assets = List())).value

          val fromAddresses = boxReferences.map(_._1)

          val expectedBoxNonces = boxReferences.map(_._2).sorted

          val result =
            underTest
              .build(
                Chain.fromSeq(fromAddresses),
                feeOutput,
                outputs,
                feeValue,
                timestamp,
                None,
                minting,
                BoxSelectionAlgorithms.All
              )
              .value
              .runA(boxSet)
              .value
              .value

          val resultBoxNonces = result.inputs.map(_._2).sorted

          resultBoxNonces shouldBe expectedBoxNonces
        }
      }

      it("should return a transaction with only specific box IDs when using the specific strategy") {
        forAll(
          boxSetGen,
          Gen.option(ModelGen.arbitraryPolyOutput.arbitrary),
          polyAndArbitOutputs,
          ModelGen.arbitraryInt128.arbitrary,
          Gen.long,
          Gen.oneOf(true, false)
        ) { (boxSet, feeOutput, outputs, feeValue, timestamp, minting) =>
          val boxesToUse = boxSet.polys.headOption.map(poly => List(poly)).getOrElse(List())
          val expectedBoxNonces = boxesToUse.map(_._2.nonce)
          val boxIds = boxesToUse.map(_._2.id)
          val fromAddresses = boxesToUse.map(_._1.toDionAddress.value)

          val result =
            underTest
              .build(
                Chain.fromSeq(fromAddresses),
                feeOutput,
                outputs,
                feeValue,
                timestamp,
                None,
                minting,
                BoxSelectionAlgorithms.Specific(boxIds)
              )
              .value
              .runA(boxSet)
              .value
              .value

          val resultBoxNonces = result.inputs.map(_._2).sorted

          resultBoxNonces shouldBe expectedBoxNonces
        }
      }

      it("should return a transaction with only asset nonces of asset codes in outputs") {
        forAll(
          boxSetGen,
          Gen.option(ModelGen.arbitraryPolyOutput.arbitrary),
          polyAndArbitOutputs,
          ModelGen.arbitraryInt128.arbitrary,
          Gen.long,
          Gen.oneOf(true, false)
        ) { (boxSet, feeOutput, outputs, feeValue, timestamp, minting) =>
          val assetBox = boxSet.assets.headOption

          val assetBoxRef = assetBox.map(box => box._1 -> box._2.nonce)

          val fromAddresses = BoxSet.toBoxReferences(boxSet).value.map(_._1)

          val outputsWithAssetOutput =
            assetBox
              .map(box => box._2.value.toAssetOutput(box._1.toDionAddress.value).value)
              .fold(outputs)(outputs.append)

          val polyAndArbitBoxReferences = BoxSet.toBoxReferences(boxSet.copy(assets = List())).value

          val expectedNonces =
            assetBoxRef
              .map(ref => ref._1.toDionAddress.value -> ref._2)
              .fold(polyAndArbitBoxReferences)(polyAndArbitBoxReferences.appended)
              .map(_._2)
              .sorted

          val result =
            underTest
              .build(
                Chain.fromSeq(fromAddresses),
                feeOutput,
                outputsWithAssetOutput,
                feeValue,
                timestamp,
                None,
                minting,
                BoxSelectionAlgorithms.All
              )
              .value
              .runA(boxSet)
              .value
              .value

          val resultBoxNonces = result.inputs.map(_._2).sorted

          resultBoxNonces shouldBe expectedNonces
        }
      }
    }
  }
}
