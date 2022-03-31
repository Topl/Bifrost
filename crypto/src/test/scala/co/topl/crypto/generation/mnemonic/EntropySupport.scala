package co.topl.crypto.generation.mnemonic

import cats.Eq
import co.topl.models.Bytes
import org.scalacheck.Arbitrary

trait EntropySupport {

  implicit val arbitraryEntropy: Arbitrary[Entropy] =
    Arbitrary.apply[Entropy](Arbitrary.arbUuid.arbitrary.map(Entropy.fromUuid))

  implicit val entropyEq: Eq[Entropy] = (a, b) => Bytes(a.value) == Bytes(b.value)
}

object EntropySupport extends EntropySupport
