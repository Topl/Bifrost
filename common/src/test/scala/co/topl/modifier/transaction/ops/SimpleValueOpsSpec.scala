package co.topl.modifier.transaction.ops

import co.topl.models.{Box => TetraBox, FullAddress, ModelGenerators, SpendingAddress, Transaction}
import co.topl.modifier.box.SimpleValue
import co.topl.modifier.implicits._
import co.topl.utils.{CommonGenerators, Int128}
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SimpleValueOpsSpec
    extends AnyFunSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with CommonGenerators {

  // need to avoid duplicate inheritance of latin1DataGen
  object ModelGen extends ModelGenerators

  describe("SimpleValueOps") {
    describe("toPolyOutput") {
      it("should convert to a poly output with the same quantity") {
        forAll(positiveInt128Gen, ModelGen.arbitraryFullAddress.arbitrary) { (value: Int128, address: FullAddress) =>
          val simpleValue = SimpleValue(value)

          val Transaction.Output(_, polyValue: TetraBox.Values.Poly, _) =
            simpleValue.toPolyOutput(address).value

          polyValue.quantity.data shouldBe BigInt(simpleValue.quantity.toByteArray)
        }
      }

      it("should convert to a poly output with the expected address") {
        forAll(positiveInt128Gen, ModelGen.arbitraryFullAddress.arbitrary) { (value: Int128, address: FullAddress) =>
          val simpleValue = SimpleValue(value)

          val polyOutput = simpleValue.toPolyOutput(address)

          polyOutput.value.address shouldBe address
        }
      }
    }

    describe("toArbitOutput") {
      it("should convert to an arbit output with the same quantity") {
        forAll(positiveInt128Gen, ModelGen.arbitraryFullAddress.arbitrary) { (value: Int128, address: FullAddress) =>
          val simpleValue = SimpleValue(value)

          val Transaction.Output(_, v, _) =
            simpleValue.toArbitOutput(address).value

          val arbitValue = v.asInstanceOf[TetraBox.Values.Arbit]

          arbitValue.quantity.data shouldBe BigInt(simpleValue.quantity.toByteArray)
        }
      }

      it("should convert to an arbit output with the expected address") {
        forAll(positiveInt128Gen, ModelGen.arbitraryFullAddress.arbitrary) { (value: Int128, address: FullAddress) =>
          val simpleValue = SimpleValue(value)

          val output = simpleValue.toArbitOutput(address)

          output.value.address shouldBe address
        }
      }
    }
  }
}
