package co.topl.interpreters

import cats.effect.IO
import cats.effect.Sync
import cats.implicits._
import co.topl.codecs.bytes.typeclasses.Persistable
import com.google.protobuf.ByteString
import fs2.io.{file => fs2file}
import munit.CatsEffectSuite

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

class CatsSecureStoreSpec extends CatsEffectSuite {

  import CatsSecureStoreSpec._

  type F[A] = IO[A]

  test("list the files in a directory") {
    fs2file
      .Files[F]
      .tempDirectory
      .use(testDir =>
        for {
          dir <- Sync[F].blocking {
            val dir1 = Paths.get(testDir.toString, "list")
            Files.createDirectories(dir1)
            Files.write(Paths.get(dir1.toString, "file1.txt"), "test1".getBytes(StandardCharsets.UTF_8))
            Files.write(Paths.get(dir1.toString, "file2.txt"), "test2".getBytes(StandardCharsets.UTF_8))
            Files.write(Paths.get(dir1.toString, "file3.txt"), "test3".getBytes(StandardCharsets.UTF_8))
            dir1
          }
          underTest = CatsSecureStore.make[F](dir)
          chain <- underTest.use(_.list)
          _ = assertEquals(chain.toList.toSet, Set("file1.txt", "file2.txt", "file3.txt"))
        } yield ()
      )
  }

  test("write a file") {
    val name = "file.txt"
    val data = TestData(Array.fill[Byte](32)(1))
    fs2file
      .Files[F]
      .tempDirectory
      .use(testDir =>
        for {
          _ <- Sync[F].blocking {
            Files.write(Paths.get(testDir.toString, name), data.value)
          }
          underTest = CatsSecureStore.make[F](testDir.toNioPath)
          _         <- underTest.use(_.write(name, data))
          fileBytes <- Sync[F].blocking(Files.readAllBytes(Paths.get(testDir.toString, name)))
          _ = assertEquals(fileBytes.length, 32)
          _ = assert(fileBytes.forall(_ === (1: Byte)))
        } yield ()
      )
  }

  test("consume a file in a directory") {
    val name = "file.txt"
    val data = TestData(Array.fill[Byte](32)(1))
    fs2file
      .Files[F]
      .tempDirectory
      .use(testDir =>
        for {
          _ <- Sync[F].blocking {
            Files.write(Paths.get(testDir.toString, name), data.value)
          }
          underTest = CatsSecureStore.make[F](testDir.toNioPath)
          dataOut <- underTest.use(_.consume[TestData](name)).map(_.get)
          _ = assert(dataOut.value.length === 32)
          _ = assert(dataOut.value.forall(_ === (1: Byte)))
          _ <- Sync[F].blocking(Files.exists(Paths.get(testDir.toString, name))).assertEquals(false)
        } yield ()
      )
  }

  test("erase a file in a directory") {
    val name = "file.txt"
    val data = TestData(Array.fill[Byte](32)(1))
    fs2file
      .Files[F]
      .tempDirectory
      .use(testDir =>
        for {
          _ <- Sync[F].blocking {
            Files.write(Paths.get(testDir.toString, name), data.value)
          }
          underTest = CatsSecureStore.make[F](testDir.toNioPath)
          _ <- underTest.use(_.erase(name))
          _ <- Sync[F].blocking(Files.exists(Paths.get(testDir.toString, name))).assertEquals(false)
        } yield ()
      )
  }

}

object CatsSecureStoreSpec {

  case class TestData(value: Array[Byte])

  implicit val persistableTestData: Persistable[TestData] =
    new Persistable[TestData] {
      def persistedBytes(value: TestData): ByteString = ByteString.copyFrom(value.value)

      def fromPersistedBytes(bytes: ByteString): Either[String, TestData] = Right(TestData(bytes.toByteArray))
    }
}
