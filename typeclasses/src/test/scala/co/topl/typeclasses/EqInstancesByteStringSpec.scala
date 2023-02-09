package co.topl.typeclasses

import cats.Eq
import com.google.protobuf.ByteString
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * ByteString Eq alternatives Spec, could be removed if it adds no value, current impl: Eq.fromUniversalEquals
 */
class EqInstancesByteStringSpec extends AnyFlatSpec with Matchers with EqInstances {

  val bytesStringEqUnsLexComparator: Eq[ByteString] =
    (a, b) => ByteString.unsignedLexicographicalComparator().compare(a, b) == 0

  val bytesStringEqUniversal: Eq[ByteString] = Eq.fromUniversalEquals

  "Eq ByteString" should "compare two emptyByteArray cats eqv" in {

    val bs1 = ByteString.copyFrom(Array.emptyByteArray)
    val bs2 = ByteString.copyFrom(Array.emptyByteArray)
    assert(bytesStringEqUnsLexComparator.eqv(bs1, bs2))
  }

  "Eq ByteString" should "compare two ByteArray int.toByte" in {
    val bs1 = ByteString.copyFrom(Array.fill(3)(0.toByte))
    val bs2 = ByteString.copyFrom(Array.fill(3)(0.toByte))
    assert(bytesStringEqUnsLexComparator.eqv(bs1, bs2))
  }

  "Eq ByteString" should "compare two ByteArray comparator" in {
    val bs1 = ByteString.copyFrom(Array.fill(3)(0.toByte))
    val bs2 = ByteString.copyFrom(Array.fill(3)(0.toByte))
    assert(bytesStringEqUnsLexComparator.eqv(bs1, bs2))
  }

  "Eq ByteString" should "compare two ByteArray comparator -1 and 1 with ==" in {
    val bs1 = ByteString.copyFrom(Array[Byte](-1))
    val bs2 = ByteString.copyFrom(Array[Byte](1))
    assert(bytesStringEqUnsLexComparator.neqv(bs1, bs2))
  }

  "Eq ByteString" should "compare two ByteArray comparator -1 and 1 with unsignedLexicographical" in {
    val bs1 = ByteString.copyFrom(Array[Byte](-1))
    val bs2 = ByteString.copyFrom(Array[Byte](1))
    assert(bytesStringEqUnsLexComparator.neqv(bs1, bs2))
  }

  "Eq ByteString" should "compare two ByteArray comparator fromUniversalEquals" in {
    val bs1 = ByteString.copyFrom(Array.fill(3)(0.toByte))
    val bs2 = ByteString.copyFrom(Array.fill(3)(0.toByte))
    assert(bytesStringEqUniversal.eqv(bs1, bs2))
  }

  "Eq ByteString" should "compare two ByteArray comparator -1 and 1 with fromUniversalEquals" in {
    val bs1 = ByteString.copyFrom(Array[Byte](-1))
    val bs2 = ByteString.copyFrom(Array[Byte](1))
    assert(bytesStringEqUniversal.neqv(bs1, bs2))
  }

}
