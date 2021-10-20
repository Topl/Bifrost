package co.topl.utils.codecs.binary

import cats.{Eq, Show}
import co.topl.utils.EqMatcher
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.codecs.binary.legacy.BifrostSerializer
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.Codec
import scodec.bits.BitVector

trait CodecCompatabilityBehavior extends AnyFlatSpec with EqMatcher with ScalaCheckDrivenPropertyChecks {

  def codecCompatabilityBehavior[T: Eq: Show](
    name:       String,
    codec:      Codec[T],
    serializer: BifrostSerializer[T],
    generator:  Gen[T]
  ): Unit = {
    behavior of (name + " serialization compatability")

    it should "generate output of codec encode that can be decoded by Bifrost serializer" in {
      forAll(generator) { value =>
        import cats.implicits._
        import co.topl.utils.codecs.binary.implicits._
        import co.topl.utils.StringDataTypes.implicits._

        val encodedValue = codec.encode(value).getOrThrow()

        val decodedValue = serializer.parseBytes(encodedValue.toByteArray).getOrThrow()

        decodedValue should eqShow(value)
      }
    }

    it should "generate output of Bifrost serializer encode that can be decoded by codec" in {
      forAll(generator) { value =>
        val encodedValue = serializer.toBytes(value)

        val decodedValue = codec.decode(BitVector(encodedValue)).getOrThrow().value

        decodedValue should eqShow(value)
      }
    }

    it should "be able to decode an encoded value" in {
      forAll(generator) { value =>
        val encodedValue = codec.encode(value).getOrThrow()

        val decodedValue = codec.decode(encodedValue).getOrThrow().value

        decodedValue should eqShow(decodedValue)
      }
    }

    it should "have no bits left over when decoding an encoded value" in {
      forAll(generator) { value =>
        val encodedValue = codec.encode(value).getOrThrow()

        val remaining = codec.decode(encodedValue).getOrThrow().remainder

        remaining should have length 0
      }
    }
  }
}
