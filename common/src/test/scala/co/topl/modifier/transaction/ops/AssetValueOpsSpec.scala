package co.topl.modifier.transaction.ops

import co.topl.models.ModelGenerators
import co.topl.utils.CommonGenerators
import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import co.topl.modifier.implicits._

class AssetValueOpsSpec
    extends AnyFunSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with CommonGenerators {

  // need to avoid duplicate inheritance of latin1DataGen
  object ModelGen extends ModelGenerators

  describe("AssetValueOps") {
    describe("toAssetValue") {
      it("should convert an Asset Value and Dion Address to an Asset Output with the same quantity") {
        forAll(assetValueGen, ModelGen.arbitraryDionAddress.arbitrary) { (assetValue, address) =>
          val assetOutput = assetValue.toAssetOutput(address)

          assetOutput.value.value.quantity.data shouldBe BigInt(assetValue.quantity.toByteArray)
        }
      }

      it("should convert an Asset Value and Dion Address to an Asset Output with the same address") {
        forAll(assetValueGen, ModelGen.arbitraryDionAddress.arbitrary) { (assetValue, address) =>
          val assetOutput = assetValue.toAssetOutput(address)

          assetOutput.value.dionAddress shouldBe address
        }
      }

      it("should convert an Asset Value and Dion Address to an Asset Output with the same version") {
        forAll(assetValueGen, ModelGen.arbitraryDionAddress.arbitrary) { (assetValue, address) =>
          val assetOutput = assetValue.toAssetOutput(address)

          assetOutput.value.value.assetCode.version shouldBe assetValue.assetCode.version
        }
      }

      it("should convert an Asset Value and Dion Address to an Asset Output with the same asset code short name") {
        forAll(assetValueGen, ModelGen.arbitraryDionAddress.arbitrary) { (assetValue, address) =>
          val assetOutput = assetValue.toAssetOutput(address)

          assetOutput.value.value.assetCode.shortName.data.bytes shouldBe assetValue.assetCode.shortName.value
        }
      }

      it("should convert an Asset Value and Dion Address to an Asset Output with the same asset code issuer") {
        forAll(assetValueGen, ModelGen.arbitraryDionAddress.arbitrary) { (assetValue, address) =>
          val expectedAddressBytes =
            assetValue.assetCode.issuer.networkPrefix +: assetValue.assetCode.issuer.evidence.evBytes

          val assetOutput = assetValue.toAssetOutput(address)

          assetOutput.value.value.assetCode.issuer.allBytes.toArray shouldBe expectedAddressBytes
        }
      }

      it("should convert an Asset Value and Dion Address to an Asset Output with the same security root") {
        forAll(assetValueGen, ModelGen.arbitraryDionAddress.arbitrary) { (assetValue, address) =>
          val assetOutput = assetValue.toAssetOutput(address)

          assetOutput.value.value.securityRoot.toArray shouldBe assetValue.securityRoot.root
        }
      }

      it("should convert an Asset Value and Dion Address to an Asset Output with the same metadata") {
        forAll(assetValueGen, ModelGen.arbitraryDionAddress.arbitrary) { (assetValue, address) =>
          val expectedMetadataBytes = assetValue.metadata.map(_.value).getOrElse(Array.empty)

          val assetOutput = assetValue.toAssetOutput(address)

          val outputMetadataBytes = assetOutput.value.value.metadata.map(_.data.bytes).getOrElse(Array.empty)

          outputMetadataBytes shouldBe expectedMetadataBytes
        }
      }
    }
  }

}
