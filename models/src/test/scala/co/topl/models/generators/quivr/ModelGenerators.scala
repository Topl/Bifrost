package co.topl.models.generators.quivr

import co.topl.models.generators.common.ModelGenerators._
import co.topl.models.utility.Lengths
import co.topl.models.utility.Lengths._
import org.scalacheck.Arbitrary
import quivr.models.Digest.{Digest32, Digest64}

trait ModelGenerators {

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
