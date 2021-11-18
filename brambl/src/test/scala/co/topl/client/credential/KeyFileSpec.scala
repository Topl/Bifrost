package co.topl.client.credential

import co.topl.crypto.signing.Password
import co.topl.models.{Bytes, DionAddress, NetworkPrefix, TypedEvidence}
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Length, Lengths, Sized}
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class KeyFileSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues {

  implicit private def sizedBytesArbitrary[L <: Length](implicit l: L): Arbitrary[Sized.Strict[Bytes, L]] =
    Arbitrary(
      Gen
        .containerOfN[Array, Byte](l.value, Gen.chooseNum[Byte](Byte.MinValue, Byte.MaxValue))
        .map(arr => Sized.strictUnsafe[Bytes, L](Bytes(arr)))
    )

  case class PasswordImpl(password: Password)

  implicit private val arbitraryPassword: Arbitrary[PasswordImpl] =
    Arbitrary(Gen.alphaNumStr.map(PasswordImpl))

  behavior of "KeyFile"

  it should "encrypt and decrypt data" in {
    forAll(
      Gen.nonEmptyContainerOf[Array, Byte](Gen.chooseNum[Byte](Byte.MinValue, Byte.MaxValue)).map(Bytes(_)),
      sizedBytesArbitrary[Lengths.`32`.type].arbitrary,
      arbitraryPassword.arbitrary
    ) { (secretData, addressData, password) =>
      whenever(password.password.nonEmpty) {
        val address = DionAddress(NetworkPrefix(1: Byte), TypedEvidence(1: Byte, addressData))

        val keyFile = KeyFile.Encryption.encrypt(secretData, address, password.password)

        val decryptedData = KeyFile.Encryption.decrypt(keyFile, password.password).value

        decryptedData shouldBe secretData
      }
    }
  }

}
