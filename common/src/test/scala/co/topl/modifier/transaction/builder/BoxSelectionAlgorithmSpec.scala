package co.topl.modifier.transaction.builder

import co.topl.modifier.transaction.builder.TransferRequests.{ArbitTransferRequest, PolyTransferRequest}
import co.topl.utils.CommonGenerators
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class BoxSelectionAlgorithmSpec
    extends AnyFlatSpec
    with CommonGenerators
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with MockFactory
    with EitherValues {

  "BoxSelectionAlgorithm.pickBoxes" should "return all provided boxes when using 'All' algorithm" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstBox, otherBoxes, address) =>
      val polyBoxes = (firstBox :: otherBoxes).map(address -> _)
      val tokenBoxes = BoxMap(Nil, polyBoxes, Nil)

      val result = BoxSelectionAlgorithm.pickBoxes(BoxSelectionAlgorithms.All, tokenBoxes, 0, 0, Map.empty)

      result.polys should contain allElementsOf polyBoxes
    }
  }

  it should "return specific poly box when using 'Specific' algorithm with existing poly boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstBox, otherBoxes, address) =>
      val polyBoxes = (firstBox :: otherBoxes).map(address -> _)
      val tokenBoxes = BoxMap(Nil, polyBoxes, Nil)
      val algorithm = BoxSelectionAlgorithms.Specific(List(firstBox.id))

      val result = BoxSelectionAlgorithm.pickBoxes(algorithm, tokenBoxes, 0, 0, Map.empty)

      result.polys should contain only (address -> firstBox)
    }
  }

  it should "return specific poly and arbit boxes when using 'Specific' algorithm with poly and arbit boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen) {
      (firstPolyBox, otherPolyBoxes, firstArbitBox, otherArbitBoxes, address) =>
        val polyBoxes = (firstPolyBox :: otherPolyBoxes).map(address -> _)
        val arbitBoxes = (firstArbitBox :: otherArbitBoxes).map(address -> _)
        val tokenBoxes = BoxMap(arbitBoxes, polyBoxes, Nil)
        val request = ArbitTransferRequest(List(address), List(address -> 100), address, address, 0, None)
        val algorithm = BoxSelectionAlgorithms.Specific(List(firstPolyBox.id, firstArbitBox.id))

        val result = BoxSelectionAlgorithm.pickBoxes(algorithm, tokenBoxes, 0, 0, Map.empty)

        result.polys should contain only (address  -> firstPolyBox)
        result.arbits should contain only (address -> firstArbitBox)
    }
  }

  it should "return specific poly and asset boxes when using 'Specific' algorithm for Asset Transfer" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), assetBoxGen, Gen.listOf(assetBoxGen), addressGen) {
      (firstPolyBox, otherPolyBoxes, firstAssetBox, otherAssetBoxes, address) =>
        val polyBoxes = (firstPolyBox :: otherPolyBoxes).map(address -> _)
        val assetBoxes = (firstAssetBox :: otherAssetBoxes).map(address -> _)
        val tokenBoxes = BoxMap(Nil, polyBoxes, assetBoxes)
        val algorithm = BoxSelectionAlgorithms.Specific(List(firstPolyBox.id, firstAssetBox.id))

        val result = BoxSelectionAlgorithm.pickBoxes(algorithm, tokenBoxes, 0, 0, Map.empty)

        result.polys should contain only (address  -> firstPolyBox)
        result.assets should contain only (address -> firstAssetBox)
    }
  }

  val random = new Random()

  it should "return the smallest poly box when choosing 'Smallest' algorithm with existing poly boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstBox, otherBoxes, address) =>
      val polyBoxes = random.shuffle(firstBox :: otherBoxes).map(address -> _)

      val smallestBox = polyBoxes.minBy(_._2.value.quantity)

      val tokenBoxes = BoxMap(Nil, polyBoxes, Nil)
      val algorithm = BoxSelectionAlgorithms.SmallestFirst

      val result = BoxSelectionAlgorithm.pickBoxes(algorithm, tokenBoxes, smallestBox._2.value.quantity, 0, Map.empty)

      result.polys should contain only smallestBox
    }
  }

  it should "return the smallest poly and arbit boxes when choosing 'Smallest' with poly and arbit boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen) {
      (firstPolyBox, otherPolyBoxes, firstArbitBox, otherArbitBoxes, address) =>
        val polyBoxes = random.shuffle(firstPolyBox :: otherPolyBoxes).map(address -> _)
        val smallestPolyBox = polyBoxes.minBy(_._2.value.quantity)

        val arbitBoxes = random.shuffle(firstArbitBox :: otherArbitBoxes).map(address -> _)
        val smallestArbitBox = arbitBoxes.minBy(_._2.value.quantity)

        val tokenBoxes = BoxMap(arbitBoxes, polyBoxes, Nil)
        val algorithm = BoxSelectionAlgorithms.SmallestFirst

        val result = BoxSelectionAlgorithm.pickBoxes(
          algorithm,
          tokenBoxes,
          smallestPolyBox._2.value.quantity,
          smallestArbitBox._2.value.quantity,
          Map.empty
        )

        result.polys should contain only smallestPolyBox
        result.arbits should contain only smallestArbitBox
    }
  }

  it should "return the smallest poly and asset boxes when choosing 'Smallest' algorithm with poly and asset boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), assetBoxGen, Gen.listOf(assetBoxGen), addressGen) {
      (firstPolyBox, otherPolyBoxes, firstAssetBox, otherAssetBoxes, address) =>
        val polyBoxes = random.shuffle(firstPolyBox :: otherPolyBoxes).map(address -> _)
        val smallestPolyBox = polyBoxes.minBy(_._2.value.quantity)

        val assetBoxes = random.shuffle(firstAssetBox :: otherAssetBoxes).map(address -> _)
        val smallestAssetBox = assetBoxes.minBy(_._2.value.quantity)

        val tokenBoxes = BoxMap(Nil, polyBoxes, assetBoxes)
        val algorithm = BoxSelectionAlgorithms.SmallestFirst

        val result = BoxSelectionAlgorithm.pickBoxes(
          algorithm,
          tokenBoxes,
          smallestPolyBox._2.value.quantity,
          0,
          Map(smallestAssetBox._2.value.assetCode -> smallestAssetBox._2.value.quantity)
        )

        result.polys should contain only smallestPolyBox
        result.assets should contain only smallestAssetBox
    }
  }

  it should "return the largest poly box when choosing 'Largest' algorithm with poly boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), addressGen) { (firstBox, otherBoxes, address) =>
      val polyBoxes = random.shuffle(firstBox :: otherBoxes).map(address -> _)

      val largestBox = polyBoxes.maxBy(_._2.value.quantity)

      val tokenBoxes = BoxMap(Nil, polyBoxes, Nil)
      val algorithm = BoxSelectionAlgorithms.LargestFirst

      val result = BoxSelectionAlgorithm.pickBoxes(algorithm, tokenBoxes, largestBox._2.value.quantity, 0, Map.empty)

      result.polys should contain only largestBox
    }
  }

  it should "return the largest poly and arbit boxes when choosing 'Largest' algorithm with poly and arbit boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), arbitBoxGen, Gen.listOf(arbitBoxGen), addressGen) {
      (firstPolyBox, otherPolyBoxes, firstArbitBox, otherArbitBoxes, address) =>
        val polyBoxes = random.shuffle(firstPolyBox :: otherPolyBoxes).map(address -> _)
        val largestPolyBox = polyBoxes.maxBy(_._2.value.quantity)

        val arbitBoxes = random.shuffle(firstArbitBox :: otherArbitBoxes).map(address -> _)
        val largestArbitbox = arbitBoxes.maxBy(_._2.value.quantity)

        val tokenBoxes = BoxMap(arbitBoxes, polyBoxes, Nil)
        val algorithm = BoxSelectionAlgorithms.LargestFirst

        val result = BoxSelectionAlgorithm.pickBoxes(
          algorithm,
          tokenBoxes,
          largestPolyBox._2.value.quantity,
          largestArbitbox._2.value.quantity,
          Map.empty
        )

        result.polys should contain only largestPolyBox
        result.arbits should contain only largestArbitbox
    }
  }

  it should "return the largest poly and asset boxes when choosing 'Largest' algorithm with poly and asset boxes" in {
    forAll(polyBoxGen, Gen.listOf(polyBoxGen), assetBoxGen, Gen.listOf(assetBoxGen), addressGen) {
      (firstPolyBox, otherPolyBoxes, firstAssetBox, otherAssetBoxes, address) =>
        val polyBoxes = random.shuffle(firstPolyBox :: otherPolyBoxes).map(address -> _)
        val largestPolyBox = polyBoxes.maxBy(_._2.value.quantity)

        val assetBoxes = random.shuffle(firstAssetBox :: otherAssetBoxes).map(address -> _)
        val largestAssetBox = assetBoxes.maxBy(_._2.value.quantity)

        val tokenBoxes = BoxMap(Nil, polyBoxes, assetBoxes)

        val algorithm = BoxSelectionAlgorithms.LargestFirst

        val result = BoxSelectionAlgorithm.pickBoxes(
          algorithm,
          tokenBoxes,
          largestPolyBox._2.value.quantity,
          0,
          Map(largestAssetBox._2.value.assetCode -> largestAssetBox._2.value.quantity)
        )

        result.polys should contain only largestPolyBox
        result.assets should contain only largestAssetBox
    }
  }
}
