package co.topl.crypto.utils

import co.topl.crypto.generation.mnemonic.{MnemonicSize, MnemonicSizes}
import co.topl.models.Bytes
import javassist.bytecode.Mnemonic
import org.scalacheck.{Arbitrary, Gen}

object Generators {
  def genRandomlySizedByteArray: Gen[Array[Byte]] = Gen.listOf(Arbitrary.arbitrary[Byte]).map(_.toArray)

  def genRandomlySizedBytes: Gen[Bytes] = genRandomlySizedByteArray.map(Bytes(_))

  def genByteArrayWithBoundedSize(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    Gen
      .choose(minSize, maxSize)
      .flatMap(sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]))
      .retryUntil(list => list.length >= minSize && list.length <= maxSize)
      .map(_.toArray)

  def genBytesWithBoundedSize(minSize: Int, maxSize: Int): Gen[Bytes] =
    genByteArrayWithBoundedSize(minSize, maxSize).map(Bytes(_))

  def genByteArrayOfSize(n: Int): Gen[Array[Byte]] =
    Gen.listOfN(n, Arbitrary.arbitrary[Byte]).retryUntil(_.length == n).map(_.toArray)

  lazy val stringGen: Gen[String] = Gen.alphaNumStr.suchThat(_.nonEmpty)

  def pickMnemonicSize(int: Int): MnemonicSize = int % 5 match {
    case 0 => MnemonicSizes.`12`
    case 1 => MnemonicSizes.`15`
    case 2 => MnemonicSizes.`18`
    case 3 => MnemonicSizes.`21`
    case 4 => MnemonicSizes.`24`
  }

}
