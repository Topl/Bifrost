package co.topl.codecs.bytes

import cats.{Eq, Show}
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.Codec

trait CodecSpec extends AnyFlatSpec with Matchers with EqMatcher with EitherValues with ScalaCheckDrivenPropertyChecks {

  def codecBehavior[T: Eq: Show](
    name:      String,
    codec:     Codec[T],
    generator: Gen[T]
  ): Unit = {
    behavior of (name + " codec")

    it should "be able to decode an encoded value" in {
      forAll(generator) { value =>
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
}
