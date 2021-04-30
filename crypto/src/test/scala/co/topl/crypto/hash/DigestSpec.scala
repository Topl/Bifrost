package co.topl.crypto.hash

import org.scalatest.propspec.AnyPropSpec
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalamock.clazz.Mock

class DigestSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

    lazy val nonEmptyBytesGen: Gen[Array[Byte]] = Gen
        .nonEmptyListOf(Arbitrary.arbitrary[Byte])
        .map(_.toArray)
        .retryUntil(_.length > 0)

    case class MockDigest(bytes: Array[Byte])

    implicit val mockDigestImpl = new Digest[MockDigest] {
        override val size: Int = 10
        override def from(b: Array[Byte]): MockDigest = MockDigest(Array[Byte]())
        override def bytes(d: MockDigest): Array[Byte] = d.bytes
    }

    property("concat concatenates the bytes of the two digests") {
        forAll(nonEmptyBytesGen, nonEmptyBytesGen) { (bytes1, bytes2) =>
            val (d1, d2) = (MockDigest(bytes1), MockDigest(bytes2))

            val result = Digest[MockDigest].concat(d1, d2)

            result shouldBe (bytes1 ++ bytes2)
        }
    }

    property("sameElements returns true when bytes of the two digests are the same") {
        forAll(nonEmptyBytesGen) { (bytes) =>
            val (d1, d2) = (MockDigest(bytes.clone()), MockDigest(bytes.clone()))

            val result = Digest[MockDigest].sameElements(d1, d2)

            result shouldBe true
        }
    }

    property("sameElements returns false when bytes of the two digests are not the same") {
        forAll(nonEmptyBytesGen, nonEmptyBytesGen) { (bytes1, bytes2) =>
            val (d1, d2) = (MockDigest(bytes1), MockDigest(bytes2))

            val result = Digest[MockDigest].sameElements(d1, d2)

            result shouldBe false
        }
    }
}
