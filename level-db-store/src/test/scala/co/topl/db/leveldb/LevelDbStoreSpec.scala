package co.topl.db.leveldb

import cats.effect.{IO, Resource}
import co.topl.codecs.bytes.typeclasses.Persistable
import co.topl.codecs.bytes.typeclasses.implicits._
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import scodec.{codecs, Codec}

import fs2.io.file.{Files, Path}
import java.util.InputMismatchException

class LevelDbStoreSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  import SpecKey._
  import SpecValue._

  ResourceFixture[Path](Resource.make(Files[F].createTempDirectory)(Files[F].deleteRecursively))
    .test("Save, Contains, Read, Delete") { testPath =>
      for {
        dbUnderTest <- LevelDbStore.makeDb[F](testPath)
        underTest   <- LevelDbStore.make[F, SpecKey, SpecValue](dbUnderTest)
        key = SpecKey("test1")
        keyArray = key.persistedBytes.toArray
        value = SpecValue("foo", 6458)
        valueArray = value.persistedBytes.toArray
        // The entry should not yet exist
        _ <- underTest.contains(key).assertEquals(false)
        // The entry should still be null
        _ <- IO.blocking(dbUnderTest.get(keyArray)).assertEquals(null)
        // Now write a value
        _ <- underTest.put(key, value)
        // Verify that the written value's byte array matches what we expect
        _ <- IO.blocking(dbUnderTest.get(keyArray)).map(_ sameElements valueArray).assert
        // The entry should exist
        _ <- underTest.contains(key).assertEquals(true)
        // The entry deserialized value should equal the input value
        _ <- underTest.get(key).assertEquals(Some(value))
        // Now delete the entry
        _ <- underTest.remove(key)
        // Verify that it no longer exists
        _ <- underTest.contains(key).assertEquals(false)
        // Verify directly that it no longer exists
        _ <- IO.blocking(dbUnderTest.get(keyArray)).assertEquals(null)
      } yield ()
    }

  ResourceFixture[Path](Resource.make(Files[F].createTempDirectory)(Files[F].deleteRecursively))
    .test("Malformed data throws exceptions") { testPath =>
      for {
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
