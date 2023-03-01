package co.topl.models.generators.common

import co.topl.models.utility.{Length, Sized}
import com.google.protobuf.ByteString
import org.scalacheck.{Arbitrary, Gen}
import co.topl.models.utility.HasLength.instances.byteStringLength

trait ModelGenerators {

  implicit val arbitraryByteString: Arbitrary[ByteString] =
    Arbitrary(Arbitrary.arbByte.arbitrary.map(b => Array(b)).map(ByteString.copyFrom))

  def genSizedStrictByteString[L <: Length](
    byteGen: Gen[Byte] = Gen.choose[Byte](0, 32)
  )(implicit l: L): Gen[Sized.Strict[ByteString, L]] =
    Gen
      .containerOfN[Array, Byte](l.value, byteGen)
      .map(ByteString.copyFrom)
      .map(Sized.strict[ByteString, L](_).toOption.get)

}
object ModelGenerators extends ModelGenerators
