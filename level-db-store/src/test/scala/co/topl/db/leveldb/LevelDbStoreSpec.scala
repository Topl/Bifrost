package co.topl.db.leveldb

import cats.effect.IO
import cats.implicits._
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.codecs.bytes.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory
import scodec.{codecs, Codec}

import java.nio.file.Files
import java.util.InputMismatchException

class LevelDbStoreSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  import SpecKey._
  import SpecValue._

  test("Save, Contains, Read, Delete") {
    for {
      testPath    <- Files.createTempDirectory("LevelDbStoreSpec").pure[F]
      dbUnderTest <- LevelDbStore.makeDb[F](testPath)
      underTest   <- LevelDbStore.make[F, SpecKey, SpecValue](dbUnderTest)
      key = SpecKey("test1")
      keyArray = key.persistedBytes.toArray
      value = SpecValue("foo", 6458)
      valueArray = value.persistedBytes.toArray
      _ <- underTest.contains(key).assertEquals(false)
      _ <- IO.blocking(dbUnderTest.get(keyArray)).assertEquals(null)
      _ <- underTest.put(key, value)
      _ <- IO.blocking(dbUnderTest.get(keyArray)).map(_ sameElements valueArray).assert
      _ <- underTest.contains(key).assertEquals(true)
      _ <- underTest.get(key).assertEquals(Some(value))
      _ <- underTest.remove(key)
      _ <- underTest.contains(key).assertEquals(false)
      _ <- IO.blocking(dbUnderTest.get(keyArray)).assertEquals(null)
    } yield ()
  }

  test("Malformed data throws exceptions") {
    for {
      testPath    <- Files.createTempDirectory("LevelDbStoreSpec").pure[F]
      dbUnderTest <- LevelDbStore.makeDb[F](testPath)
      underTest   <- LevelDbStore.make[F, SpecKey, SpecValue](dbUnderTest)
      key = SpecKey("test1")
      keyArray = key.persistedBytes.toArray
      _ <- IO.blocking(dbUnderTest.put(keyArray, Array[Byte](1, 2, 3, 4)))
      _ <- underTest.contains(key).assertEquals(true)
      _ <- interceptIO[InputMismatchException](underTest.get(key))
    } yield ()
  }

}

case class SpecKey(id: String)

object SpecKey {

  implicit val testKeyCodec: Codec[SpecKey] =
    codecs.utf8_32.as[SpecKey]

  implicit val testKeyPersistable: Persistable[SpecKey] =
    Persistable.instanceFromCodec
}
case class SpecValue(value1: String, value2: Long)

object SpecValue {

  implicit val specValueCodec: Codec[SpecValue] =
    (codecs.utf8_32 :: codecs.vlongL).as[SpecValue]

  implicit val specValuePersistable: Persistable[SpecValue] =
    Persistable.instanceFromCodec
}
