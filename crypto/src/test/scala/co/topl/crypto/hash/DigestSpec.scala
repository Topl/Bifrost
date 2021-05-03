package co.topl.crypto.hash

import co.topl.crypto.utils.Generators._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

trait DigestSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  def testsForDigest[D: Digest](name: String): Unit = {
    property(s"$name concat concatenates the bytes of the two digests") {
      forAll(genRandomlySizedByteArray, genRandomlySizedByteArray) { (bytes1, bytes2) =>
        val (d1, d2) = (Digest[D].from(bytes1), Digest[D].from(bytes2))

        val result = Digest[D].concat(d1, d2)

        result shouldBe (bytes1 ++ bytes2)
      }
    }

    property(s"$name sameElements returns true when bytes of the two digests are the same") {
      forAll(genRandomlySizedByteArray) { (bytes) =>
        val (d1, d2) = (Digest[D].from(bytes.clone()), Digest[D].from(bytes.clone()))

        val result = Digest[D].sameElements(d1, d2)

        result shouldBe true
      }
    }

    property(s"$name sameElements returns false when bytes of the two digests are not the same") {
      forAll(genRandomlySizedByteArray, genRandomlySizedByteArray) { (bytes1, bytes2) =>
        val (d1, d2) = (Digest[D].from(bytes1), Digest[D].from(bytes2))

        val result = Digest[D].sameElements(d1, d2)

        result shouldBe (bytes1 sameElements bytes2)
      }
    }
  }

}
