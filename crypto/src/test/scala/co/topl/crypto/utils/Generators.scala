package co.topl.crypto.utils

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

object Generators {
  def genRandomlySizedByteArray: Gen[Array[Byte]] = Gen.listOf(Arbitrary.arbitrary[Byte]).map(_.toArray)

  def genByteArrayWithBoundedSize(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    Gen
      .choose(minSize, maxSize)
      .flatMap(sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]))
      .retryUntil(list => list.length >= minSize && list.length <= maxSize)
      .map(_.toArray)

  def genByteArrayOfSize(n: Int): Gen[Array[Byte]] =
    Gen.listOfN(n, Arbitrary.arbitrary[Byte]).retryUntil(_.length == n).map(_.toArray)

  def smallInt: Gen[Int] = Gen.choose(0, 20)
}
