package co.topl.models

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import co.topl.models.generators.common.ModelGenerators.genSizedStrictByteString
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility._
import com.google.protobuf.ByteString
import org.scalacheck.rng.Seed
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

trait ModelGenerators {

  def nonEmptyChainOf[T](gen: => Gen[T]): Gen[NonEmptyChain[T]] =
    for {
      tail <- Gen.listOf(gen)
    } yield NonEmptyChain.fromNonEmptyList(NonEmptyList(gen.first, tail))

  /**
   * Similar to Gen.nonEmptyListOf, but without the bug associated with:https://github.com/typelevel/scalacheck/issues/372
   */
  def nonEmptyListOf[T](gen: => Gen[T]): Gen[List[T]] =
    nonEmptyChainOf(gen).map(_.toNonEmptyList.toList)

  def etaGen: Gen[Eta] =
    genSizedStrictBytes[Lengths.`32`.type]()

  def bigIntGen: Gen[BigInt] = Gen.long.map(BigInt(_))

  def ratioGen: Gen[Ratio] =
    for {
      n <- bigIntGen
      d <- bigIntGen
    } yield Ratio(n, d)

  def relativeStakeGen: Gen[Ratio] =
    Gen.chooseNum(1L, 5L).flatMap(denominator => Ratio(1L, denominator))

  def latin1DataGen: Gen[Latin1Data] =
    Gen
      .containerOfN[Array, Byte](32, Gen.choose[Byte](0, 32))
      .map(Latin1Data.fromData(_))

  def vkVrfEd25519Gen: Gen[VerificationKeys.VrfEd25519] =
    genSizedStrictBytes[Lengths.`32`.type]().map(VerificationKeys.VrfEd25519(_))

  def networkPrefixGen: Gen[NetworkPrefix] =
    byteGen.map(NetworkPrefix(_))

  def vkKesSumGen: Gen[VerificationKeys.KesSum] =
    for {
      bytes <- genSizedStrictBytes[VerificationKeys.KesSum.Length]()
      step  <- Gen.posNum[Int]
    } yield VerificationKeys.KesSum(bytes, step)

  def skVrfEd25519Gen: Gen[SecretKeys.VrfEd25519] =
    genSizedStrictBytes[SecretKeys.VrfEd25519.Length]().map(SecretKeys.VrfEd25519(_))

  def kesBinaryTreeGen: Gen[KesBinaryTree] =
    Gen.chooseNum[Int](0, 2).flatMap {
      case 0 =>
        for {
          seed         <- Gen.containerOfN[Array, Byte](32, byteGen)
          witnessLeft  <- Gen.containerOfN[Array, Byte](32, byteGen)
          witnessRight <- Gen.containerOfN[Array, Byte](32, byteGen)
          left         <- kesBinaryTreeGen
          right        <- kesBinaryTreeGen
        } yield KesBinaryTree.MerkleNode(seed, witnessLeft, witnessRight, left, right)
      case 1 =>
        for {
          sk <- Gen.containerOfN[Array, Byte](32, byteGen)
          vk <- Gen.containerOfN[Array, Byte](32, byteGen)
        } yield KesBinaryTree.SigningLeaf(sk, vk)
      case 2 => Gen.const(KesBinaryTree.Empty())
    }

  def kesSumSKGen: Gen[SecretKeys.KesSum] =
    for {
      tree   <- kesBinaryTreeGen
      offset <- Gen.long
    } yield SecretKeys.KesSum(tree, offset)

  def kesProductSKGen: Gen[SecretKeys.KesProduct] =
    for {
      superTree   <- kesBinaryTreeGen
      subTree     <- kesBinaryTreeGen
      nextSubSeed <- Gen.containerOfN[Array, Byte](32, byteGen)
      signature   <- co.topl.models.generators.consensus.ModelGenerators.signatureKesSumArbitrary.arbitrary
      offset      <- Gen.long
    } yield SecretKeys.KesProduct(superTree, subTree, nextSubSeed, signature, offset)

  def partialOperationalCertificateGen: Gen[UnsignedBlockHeader.PartialOperationalCertificate] =
    for {
      parentVK <- co.topl.models.generators.consensus.ModelGenerators.arbitraryVerificationKeyKesProduct.arbitrary
      parentSignature <- co.topl.models.generators.consensus.ModelGenerators.signatureKesProductArbitrary.arbitrary
      childVK         <- co.topl.models.generators.consensus.ModelGenerators.verificationKeyEd25519Gen
    } yield UnsignedBlockHeader.PartialOperationalCertificate(parentVK, parentSignature, childVK)

  def stakingAddressGen: Gen[StakingAddress] =
    for {
      poolVK <- genSizedStrictBytes[Lengths.`32`.type]()
    } yield poolVK.data

  def unsignedHeaderGen(
    parentHeaderIdGen: Gen[co.topl.consensus.models.BlockId] =
      co.topl.models.generators.consensus.ModelGenerators.arbitraryBlockId.arbitrary,
    parentSlotGen:  Gen[Slot] = Gen.chooseNum(0L, 50L),
    txRootGen:      Gen[ByteString] = genSizedStrictByteString[Lengths.`32`.type]().map(_.data),
    bloomFilterGen: Gen[ByteString] = genSizedStrictByteString[Lengths.`256`.type]().map(_.data),
    timestampGen:   Gen[Timestamp] = Gen.chooseNum(0L, 50L),
    heightGen:      Gen[Long] = Gen.chooseNum(0L, 20L),
    slotGen:        Gen[Slot] = Gen.chooseNum(0L, 50L),
    eligibilityCertificateGen: Gen[co.topl.consensus.models.EligibilityCertificate] =
      co.topl.models.generators.consensus.ModelGenerators.arbitraryEligibilityCertificate.arbitrary,
    partialOperationalCertificateGen: Gen[UnsignedBlockHeader.PartialOperationalCertificate] =
      partialOperationalCertificateGen,
    metadataGen: Gen[ByteString] = genSizedStrictByteString[Lengths.`32`.type]().map(_.data),
    addressGen:  Gen[ByteString] = genSizedStrictByteString[Lengths.`32`.type]().map(_.data)
  ): Gen[UnsignedBlockHeader] =
    for {
      parentHeaderID <- parentHeaderIdGen
      parentSlot     <- parentSlotGen
      txRoot         <- txRootGen
      bloomFilter    <- bloomFilterGen
      timestamp      <- timestampGen
      height         <- heightGen
      slot           <- slotGen
      vrfCertificate <- eligibilityCertificateGen
      kesCertificate <- partialOperationalCertificateGen
      metadata       <- metadataGen
      address        <- addressGen
    } yield UnsignedBlockHeader(
      parentHeaderID,
      parentSlot,
      txRoot,
      bloomFilter,
      timestamp,
      height,
      slot,
      vrfCertificate,
      kesCertificate,
      metadata,
      address
    )

  def byteGen: Gen[Byte] = Gen.choose[Byte](Byte.MinValue, Byte.MaxValue)

  def genSizedMaxBytes[L <: Length](
    byteGen: Gen[Byte] = Gen.choose[Byte](0, 32)
  )(implicit l: L): Gen[Sized.Max[Bytes, L]] =
    Gen
      .containerOfN[Array, Byte](l.value, byteGen)
      .map(ByteString.copyFrom(_))
      .map(Sized.max[Bytes, L](_).toOption.get)

  def genSizedStrictBytes[L <: Length](
    byteGen: Gen[Byte] = Gen.choose[Byte](0, 32)
  )(implicit l: L): Gen[Sized.Strict[Bytes, L]] =
    Gen
      .containerOfN[Array, Byte](l.value, byteGen)
      .map(ByteString.copyFrom(_))
      .map(Sized.strict[Bytes, L](_).toOption.get)

  implicit val arbitraryBytes: Arbitrary[Bytes] =
    Arbitrary(implicitly[Arbitrary[Array[Byte]]].arbitrary.map(ByteString.copyFrom(_)))

  implicit val arbitraryKesProductSK: Arbitrary[SecretKeys.KesProduct] =
    Arbitrary(kesProductSKGen)

  implicit def arbitraryStrictSizedBytes[L <: Length](implicit l: L): Arbitrary[Sized.Strict[Bytes, L]] =
    Arbitrary(genSizedStrictBytes[L]())

  implicit val arbitraryRho: Arbitrary[Rho] =
    Arbitrary(
      genSizedStrictBytes[Lengths.`64`.type]().map(Rho(_))
    )

  implicit val arbitraryEta: Arbitrary[Eta] =
    Arbitrary(etaGen)

  implicit val arbitraryStakingAddress: Arbitrary[StakingAddress] =
    Arbitrary(stakingAddressGen)

  implicit class GenHelper[T](gen: Gen[T]) {
    def first: T = gen.pureApply(Gen.Parameters.default, Seed.random())
  }
}

object ModelGenerators extends ModelGenerators
