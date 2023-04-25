package co.topl.crypto.utils

import co.topl.crypto.models._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

object NodeCryptoGenerators {
  def genRandomlySizedByteArray: Gen[Array[Byte]] = Gen.listOf(Arbitrary.arbitrary[Byte]).map(_.toArray)

  def genByteArrayWithBoundedSize(minSize: Int, maxSize: Int): Gen[Array[Byte]] =
    Gen
      .choose(minSize, maxSize)
      .flatMap(sz => Gen.listOfN(sz, Arbitrary.arbitrary[Byte]))
      .retryUntil(list => list.length >= minSize && list.length <= maxSize)
      .map(_.toArray)

  def genByteArrayOfSize(n: Int): Gen[Array[Byte]] =
    Gen.listOfN(n, Arbitrary.arbitrary[Byte]).retryUntil(_.length == n).map(_.toArray)

  def vkKesSumGen: Gen[VerificationKeyKesSum] =
    for {
      bytes <- byteArrayGen(32)
      step  <- Gen.posNum[Int]
    } yield VerificationKeyKesSum(bytes, step)

  def kesBinaryTreeGen: Gen[KesBinaryTree] =
    Gen.chooseNum[Int](0, 2).flatMap {
      case 0 =>
        for {
          seed         <- byteArrayGen(32)
          witnessLeft  <- byteArrayGen(32)
          witnessRight <- byteArrayGen(32)
          left         <- kesBinaryTreeGen
          right        <- kesBinaryTreeGen
        } yield KesBinaryTree.MerkleNode(seed, witnessLeft, witnessRight, left, right)
      case 1 =>
        for {
          sk <- byteArrayGen(32)
          vk <- byteArrayGen(32)
        } yield KesBinaryTree.SigningLeaf(sk, vk)
      case 2 => Gen.const(KesBinaryTree.Empty())
    }

  def kesSumSKGen: Gen[SecretKeyKesSum] =
    for {
      tree   <- kesBinaryTreeGen
      offset <- Gen.long
    } yield SecretKeyKesSum(tree, offset)

  def kesProductSKGen: Gen[SecretKeyKesProduct] =
    for {
      superTree   <- kesBinaryTreeGen
      subTree     <- kesBinaryTreeGen
      nextSubSeed <- byteArrayGen(32)
      signature   <- signatureKesSumArbitrary.arbitrary
      offset      <- Gen.long
    } yield SecretKeyKesProduct(superTree, subTree, nextSubSeed, signature, offset)

  implicit val signatureKesSumArbitrary: Arbitrary[SignatureKesSum] =
    Arbitrary(
      for {
        verificationKey <- byteArrayGen(32)
        signature       <- byteArrayGen(64)
        witness         <- Gen.nonEmptyContainerOf[Seq, Array[Byte]](byteArrayGen(32))
      } yield SignatureKesSum(verificationKey, signature, witness)
    )

  implicit val signatureKesProductArbitrary: Arbitrary[SignatureKesProduct] =
    Arbitrary(
      for {
        superSignature <- signatureKesSumArbitrary.arbitrary
        subSignature   <- signatureKesSumArbitrary.arbitrary
        subRoot        <- byteArrayGen(32)
      } yield SignatureKesProduct(superSignature, subSignature, subRoot)
    )

  def byteArrayGen(length: Int): Gen[Array[Byte]] =
    Gen.containerOfN[Array, Byte](length, Gen.choose[Byte](Byte.MinValue, Byte.MaxValue))

}
