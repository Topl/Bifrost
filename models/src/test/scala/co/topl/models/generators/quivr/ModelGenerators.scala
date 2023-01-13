package co.topl.models.generators.quivr

import co.topl.models.utility.HasLength.instances.byteStringLength
import co.topl.models.utility.{Length, Lengths, Sized}
import org.scalacheck.{Arbitrary, Gen}
import quivr.models.Digest.{Digest32, Digest64}
import co.topl.models.utility.Lengths._
import com.google.protobuf.ByteString

trait ModelGenerators {

  // TODO move this gen, if other models need it
  def genSizedStrictByteString[L <: Length](
    byteGen:    Gen[Byte] = Gen.choose[Byte](0, 32)
  )(implicit l: L): Gen[Sized.Strict[ByteString, L]] =
    Gen
      .containerOfN[Array, Byte](l.value, byteGen)
      .map(ByteString.copyFrom)
      .map(Sized.strict[ByteString, L](_).toOption.get)

  val arbitraryDigest32: Arbitrary[Digest32] =
    Arbitrary(
      for {
        bs <- genSizedStrictByteString[Lengths.`32`.type]()
      } yield Digest32(bs.data)
    )

  val arbitraryDigest64: Arbitrary[Digest64] =
    Arbitrary(
      for {
        bs <- genSizedStrictByteString[Lengths.`64`.type]()
      } yield Digest64(bs.data)
    )
}

object ModelGenerators extends ModelGenerators
