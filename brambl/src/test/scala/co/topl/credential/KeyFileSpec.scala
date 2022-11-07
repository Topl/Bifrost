package co.topl.credential

import co.topl.credential.KeyFile.Encryption.MACMismatch
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, TypedEvidence}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class KeyFileSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues {

  behavior of "KeyFile"

  private val password = "test"

  private val evidence: TypedEvidence =
    TypedEvidence(1: Byte, Sized.strictUnsafe(Bytes(Array.fill[Byte](32)(0: Byte))))

  it should "encrypt and decrypt data" in {
    forAll(
      Gen
        .nonEmptyContainerOf[Array, Byte](Gen.chooseNum[Byte](Byte.MinValue, Byte.MaxValue))
        .map(Bytes(_))
    ) { secretData =>
      val keyFile =
        KeyFile.Encryption.encrypt(secretData, KeyFile.Metadata(evidence), password)

      val decryptedData = KeyFile.Encryption.decrypt(keyFile, password).value

      decryptedData shouldBe secretData
    }
  }

  it should "encrypt and decrypt data (simple)" in {
    val secretData = Bytes(Array[Byte](1, 2, 3))

    val keyFile =
      KeyFile.Encryption.encrypt(secretData, KeyFile.Metadata(evidence), password)

    val decryptedData = KeyFile.Encryption.decrypt(keyFile, password).value

    decryptedData shouldBe secretData
  }

  it should "fail to decrypt with incorrect password" in {
    val secretData = Bytes(Array[Byte](1, 2, 3))

    val keyFile = KeyFile.Encryption.encrypt(secretData, KeyFile.Metadata(evidence), "123")

    val failure = KeyFile.Encryption.decrypt(keyFile, "456").left.value

    failure shouldBe MACMismatch
  }

}
