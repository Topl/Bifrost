package co.topl.interpreters

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.models.Bytes
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, Inspectors, OptionValues}
import scodec.bits.ByteVector

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Comparator

class AkkaSecureStoreSpec
    extends ScalaTestWithActorTestKit
    with Inspectors
    with Matchers
    with MockFactory
    with EitherValues
    with OptionValues
    with BeforeAndAfterAll
    with AnyFlatSpecLike {

  import AkkaSecureStoreSpec._

  behavior of "AkkaSecureStore"

  private var testDir: Path = _

  it should "list the files in a directory" in {
    val dir1 = Paths.get(testDir.toString, "list")
    Files.createDirectories(dir1)
    Files.write(Paths.get(dir1.toString, "file1.txt"), "test1".getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get(dir1.toString, "file2.txt"), "test2".getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get(dir1.toString, "file3.txt"), "test3".getBytes(StandardCharsets.UTF_8))

    val underTest = AkkaSecureStore.Eval.make[IO](dir1).unsafeRunSync()

    val chain = underTest.list.unsafeRunSync()

    chain.toList.toSet shouldBe Set("file1.txt", "file2.txt", "file3.txt")
  }

  it should "write a file" in {
    val name = "file.txt"
    val data = TestData(Array.fill[Byte](32)(1))
    val dir1 = Paths.get(testDir.toString, "write")
    Files.createDirectories(dir1)

    val underTest = AkkaSecureStore.Eval.make[IO](dir1).unsafeRunSync()

    underTest.write(name, data).unsafeRunSync()

    val fileBytes =
      Files.readAllBytes(Paths.get(dir1.toString, name))

    fileBytes.length shouldBe 32
    forAll(fileBytes.toList)(_ shouldBe (1: Byte))
  }

  it should "read a file in a directory" in {
    val dir1 = Paths.get(testDir.toString, "read")
    val name = "file.txt"
    val data = TestData(Array.fill[Byte](32)(1))
    Files.createDirectories(dir1)
    Files.write(Paths.get(dir1.toString, name), data.value)

    val underTest = AkkaSecureStore.Eval.make[IO](dir1).unsafeRunSync()

    val dataOut = underTest.consume[TestData](name).unsafeRunSync().value

    dataOut.value.length shouldBe 32
    forAll(dataOut.value.toList)(_ shouldBe (1: Byte))
  }

  it should "securely delete a file in a directory" in {
    val name = "file.txt"
    val data = TestData(Array.fill[Byte](32)(1))
    val dir1 = Paths.get(testDir.toString, "delete")
    Files.createDirectories(dir1)

    val underTest = AkkaSecureStore.Eval.make[IO](dir1).unsafeRunSync()

    underTest.write(name, data).unsafeRunSync()

    val dataOut = underTest.consume[TestData](name).unsafeRunSync().value

    dataOut.value.length shouldBe 32
    forAll(dataOut.value.toList)(_ shouldBe (1: Byte))

    underTest.erase(name).unsafeRunSync()

    Files.exists(Paths.get(dir1.toString, name)) shouldBe false
  }

  override def beforeAll(): Unit = {
    super.beforeAll()
    testDir = Files.createTempDirectory("AkkaSecureStoreSpec")
  }

  override def afterAll(): Unit = {
    super.afterAll()
    Files
      .walk(testDir)
      .sorted(Comparator.reverseOrder[Path]())
      .forEach(Files.delete(_))
  }
}

object AkkaSecureStoreSpec {
  case class TestData(value: Array[Byte])

  implicit val persistableTestData: Persistable[TestData] =
    new Persistable[TestData] {
      def persistedBytes(value: TestData): ByteVector = Bytes(value.value)

      def fromPersistedBytes(bytes: ByteVector): Either[String, TestData] = Right(TestData(bytes.toArray))
    }
}
