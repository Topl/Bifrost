package co.topl.codecs.bytes

import cats.effect.IO
import cats.implicits._
import cats.Eq
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF
import org.scalacheck.{Arbitrary, Gen}
import scodec.Codec

import scala.reflect.ClassTag

trait CodecSpec extends CatsEffectSuite with ScalaCheckEffectSuite {

  type F[A] = IO[A]

  def codecBehavior[T: Eq](
    name:      String,
    codec:     Codec[T],
    generator: Gen[T]
  ): Unit = {
    test(name + " codec should be able to decode an encoded value") {
      PropF.forAllF(generator)(value =>
        codec
          .encode(value)
          .toEither
          .flatMap(codec.decode(_).toEither.map(_.value))
          .map(_ === value)
          .leftMap(e => new IllegalArgumentException(e.toString))
          .pure[F]
          .rethrow
          .assert
      )
    }

    test(name + " codec should have no bits left over when decoding an encoded value") {
      PropF.forAllF(generator) { value =>
        codec
          .encode(value)
          .toEither
          .flatMap(codec.decode(_).map(_.remainder.length == 0).toEither)
          .leftMap(e => new IllegalArgumentException(e.toString))
          .pure[F]
          .rethrow
          .assert
      }
    }
  }

  def codecBehavior[T: Eq: Codec: Arbitrary: ClassTag](): Unit =
    codecBehavior[T](implicitly[ClassTag[T]].toString, implicitly[Codec[T]], implicitly[Arbitrary[T]].arbitrary)
}
