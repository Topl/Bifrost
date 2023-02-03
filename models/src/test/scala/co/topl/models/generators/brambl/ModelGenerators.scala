package co.topl.models.generators.brambl

import co.topl.brambl.models.{Evidence, Identifier}
import org.scalacheck.Arbitrary
import co.topl.models.generators.quivr.ModelGenerators._

trait ModelGenerators {

  implicit val arbitraryEvidence32: Arbitrary[Evidence.Sized32] =
    Arbitrary(
      for {
        d32 <- arbitraryDigest32.arbitrary
      } yield Evidence.Sized32.of(Some(d32))
    )

  implicit val arbitraryEvidence64: Arbitrary[Evidence.Sized64] =
    Arbitrary(
      for {
        d64 <- arbitraryDigest64.arbitrary
      } yield Evidence.Sized64.of(Some(d64))
    )

  implicit val arbitraryIoTransaction32: Arbitrary[Identifier.IoTransaction32] =
    Arbitrary(
      for {
        ev <- arbitraryEvidence32.arbitrary
      } yield Identifier.IoTransaction32.of(Some(ev))
    )

  implicit val arbitraryIoTransaction64: Arbitrary[Identifier.IoTransaction64] =
    Arbitrary(
      for {
        ev <- arbitraryEvidence64.arbitrary
      } yield Identifier.IoTransaction64.of(Some(ev))
    )

}
object ModelGenerators extends ModelGenerators
