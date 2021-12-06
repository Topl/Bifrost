package co.topl.credential

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.crypto.signing.Password
import co.topl.models.{Bytes, _}
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Length, Lengths, Sized}
import io.circe.syntax._
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.{Comparator, UUID}

class DiskCredentialIOSpec
    extends AnyFlatSpec
    with BeforeAndAfterAll
    with MockFactory
    with Matchers
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  type F[A] = IO[A]

  private var testDirectory: Path = _

  behavior of "DiskCredentialIO"

  implicit private def sizedBytesArbitrary[L <: Length](implicit l: L): Arbitrary[Sized.Strict[Bytes, L]] =
    Arbitrary(
      Gen
        .containerOfN[Array, Byte](l.value, Gen.chooseNum[Byte](0, 127))
        .map(arr => Sized.strictUnsafe[Bytes, L](Bytes(arr)))
    )

  implicit val arbitraryTypedEvidence: Arbitrary[TypedEvidence] =
    Arbitrary(
      for {
        evidence <- sizedBytesArbitrary[Lengths.`32`.type].arbitrary
        prefix   <- Gen.chooseNum[Byte](Byte.MinValue, Byte.MaxValue)
      } yield TypedEvidence(prefix, evidence)
    )

  case class PasswordImpl(password: Password)

  implicit private val arbitraryPassword: Arbitrary[PasswordImpl] =
    Arbitrary(Gen.alphaNumStr.map(PasswordImpl))

  it should "write data to disk" in {
    forAll {
      (
        evidence:     TypedEvidence,
        secretData:   Sized.Strict[Bytes, Lengths.`64`.type],
        passwordImpl: PasswordImpl
      ) =>
        val password = passwordImpl.password
        whenever(password.nonEmpty) {
          val basePath = Paths.get(testDirectory.toString, "test1")
          val underTest = DiskCredentialIO[F](basePath)

          underTest
            .write(evidence, secretData.data, password)
            .unsafeRunSync()

          val expectedKeyfilePath = Paths.get(basePath.toString, evidence.allBytes.toBase58 + ".json")

          Files.exists(expectedKeyfilePath) shouldBe true

          val fileContents = Files.readString(expectedKeyfilePath, StandardCharsets.UTF_8)

          val keyFile =
            io.circe.parser.parse(fileContents).value.as[KeyFile].value

          val decrypted = KeyFile.Encryption.decrypt(keyFile, password).value

          decrypted shouldBe secretData.data
        }
    }
  }

  it should "unlock data from disk" in {
    forAll {
      (
        evidence:     TypedEvidence,
        secretData:   Sized.Strict[Bytes, Lengths.`64`.type],
        passwordImpl: PasswordImpl
      ) =>
        val password = passwordImpl.password
        whenever(password.nonEmpty) {
          val basePath = Paths.get(testDirectory.toString, "test2")
          val underTest = DiskCredentialIO[F](basePath)

          val keyFile = KeyFile.Encryption.encrypt(
            secretData.data,
            KeyFile.Metadata(evidence),
            password
          )

          val keyFilePath = Paths.get(basePath.toString, evidence.allBytes.toBase58 + ".json")

          Files.createDirectories(keyFilePath.getParent)

          Files.write(
            keyFilePath,
            keyFile.asJson.toString.getBytes(StandardCharsets.ISO_8859_1)
          )

          val (secretBytes, _) = underTest.unlock(evidence, password).unsafeRunSync().value

          secretBytes shouldBe secretData.data
        }
    }
  }

  it should "delete data from disk" in {
    forAll {
      (
        evidence:     TypedEvidence,
        secretData:   Sized.Strict[Bytes, Lengths.`64`.type],
        passwordImpl: PasswordImpl
      ) =>
        val password = passwordImpl.password
        whenever(password.nonEmpty) {
          val basePath = Paths.get(testDirectory.toString, "test3")
          val underTest = DiskCredentialIO[F](basePath)

          val keyFile = KeyFile.Encryption.encrypt(
            secretData.data,
            KeyFile.Metadata(evidence),
            password
          )

          val keyFilePath = Paths.get(basePath.toString, evidence.allBytes.toBase58 + ".json")

          Files.createDirectories(keyFilePath.getParent)

          Files.write(
            keyFilePath,
            keyFile.asJson.toString.getBytes(StandardCharsets.ISO_8859_1)
          )

          Files.exists(keyFilePath) shouldBe true

          underTest.delete(evidence).unsafeRunSync()

          Files.exists(keyFilePath) shouldBe false
        }
    }
  }

  it should "list entries in a directory" in {
    forAll(Gen.alphaNumStr, arbitraryTypedEvidence.arbitrary) { (keyFileData, evidence) =>
      val basePath = Paths.get(testDirectory.toString, "test4" + UUID.randomUUID().toString)
      val underTest = DiskCredentialIO[F](basePath)

      val keyFilePath = Paths.get(basePath.toString, evidence.allBytes.toBase58 + ".json")

      Files.createDirectories(keyFilePath.getParent)

      Files.write(
        keyFilePath,
        keyFileData.getBytes(StandardCharsets.UTF_8)
      )

      underTest.listEvidence.unsafeRunSync() shouldBe Set(evidence)
    }
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    testDirectory = Files.createTempDirectory("DiskCredentialIOSpec")
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    import scala.jdk.CollectionConverters._
    Files
      .walk(testDirectory)
      .sorted(Comparator.reverseOrder[Path]())
      .iterator()
      .asScala
      .foreach(Files.delete)
  }

}
