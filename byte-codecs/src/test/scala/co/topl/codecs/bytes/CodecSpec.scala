package co.topl.codecs.bytes

import cats.{Eq, Show}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.Codec

import scala.reflect.ClassTag

trait CodecSpec extends AnyFlatSpec with Matchers with EqMatcher with EitherValues with ScalaCheckDrivenPropertyChecks {

  def codecBehavior[T: Eq: Show](
    name:      String,
    codec:     Codec[T],
    generator: Gen[T]
  ): Unit = {
    behavior of (name + " codec")

    it should "be able to decode an encoded value" in {
      forAll(generator) { value =>
        if (name == "ProtoInt128") {
          println(" codec.encode(value)")
          println(codec.encode(value))
        }
        if (name == "ProtoInt128") {
          println(" decode.encode(value)")
          println(codec.decode(codec.encode(value).toEither.value))
        }
        val encodedValue = codec.encode(value).toEither.value

        val decodedValue = codec.decode(encodedValue).toEither.value.value

        decodedValue should eqvShow(decodedValue)
      }
    }

    it should "have no bits left over when decoding an encoded value" in {
      forAll(generator) { value =>
        val encodedValue = codec.encode(value).toEither.value

        val remaining = codec.decode(encodedValue).toEither.value.remainder

        remaining should have length 0
      }
    }
  }

  def codecBehavior[T: Eq: Show: Codec: Arbitrary: ClassTag](): Unit =
    codecBehavior[T](implicitly[ClassTag[T]].toString, implicitly[Codec[T]], implicitly[Arbitrary[T]].arbitrary)
}
