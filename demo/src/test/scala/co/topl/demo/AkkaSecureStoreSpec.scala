package co.topl.demo

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import cats._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits._
import co.topl.crypto.keyfile.{SecureBytes, SecureData}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, Inspectors, OptionValues}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.Comparator
import scala.collection.mutable

class AkkaSecureStoreSpec
    extends ScalaTestWithActorTestKit
    with Inspectors
    with Matchers
    with MockFactory
    with EitherValues
    with OptionValues
    with BeforeAndAfterAll
    with AnyFlatSpecLike {

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
    val data = SecureData("file1.txt", SecureBytes("test1".getBytes(StandardCharsets.UTF_8)))
    val dir1 = Paths.get(testDir.toString, "write")
    Files.createDirectories(dir1)

    val underTest = AkkaSecureStore.Eval.make[IO](dir1).unsafeRunSync()

    underTest.write(data).unsafeRunSync()

    val fileString =
      new String(Files.readAllBytes(Paths.get(dir1.toString, "file1.txt")), StandardCharsets.UTF_8)

    fileString shouldBe "test1"
  }

  it should "read a file in a directory" in {
    val dir1 = Paths.get(testDir.toString, "read")
    Files.createDirectories(dir1)
    Files.write(Paths.get(dir1.toString, "file1.txt"), "test1".getBytes(StandardCharsets.UTF_8))

    val underTest = AkkaSecureStore.Eval.make[IO](dir1).unsafeRunSync()

    val data = underTest.read("file1.txt").unsafeRunSync().value

    var bytesOut: Array[Byte] = null

    data.bytes.foldLeft[Id, mutable.ArrayBuilder[Byte]](mutable.ArrayBuilder.make[Byte])(_.addOne(_))(builder =>
      (bytesOut = builder.result()).pure[Id]
    )

    val string = new String(bytesOut, StandardCharsets.UTF_8)

    string shouldBe "test1"
  }

  it should "securely delete a file in a directory" in {
    val array = "test1".getBytes(StandardCharsets.UTF_8)
    val data = SecureData("file1.txt", SecureBytes(array))
    val dir1 = Paths.get(testDir.toString, "delete")
    Files.createDirectories(dir1)

    val underTest = AkkaSecureStore.Eval.make[IO](dir1).unsafeRunSync()

    underTest.write(data).unsafeRunSync()

    val fileString =
      new String(Files.readAllBytes(Paths.get(dir1.toString, "file1.txt")), StandardCharsets.UTF_8)

    fileString shouldBe "test1"

    underTest.delete("file1.txt").unsafeRunSync()

    forAll(array.toSeq)(_ shouldBe (0: Byte))

    Files.exists(Paths.get(dir1.toString, "file1.txt")) shouldBe false
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
