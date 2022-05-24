package co.topl.codecs.binary.typeclasses

import cats.data.{Chain, NonEmptyChain}
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.Curve25519
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, ModelGenerators, SecretKeys, Transaction}
import co.topl.modifier.ops.TetraTransactionOps
import co.topl.modifier.ops.implicits._
import co.topl.typeclasses.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SignableInstancesSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with OptionValues {

  import SignableInstancesSpec._

  behavior of "SignableInstances.Instances.unprovenTransactionSignable"

  it should "generate the expected signable bits from a poly transfer" in {
    forAll(arbitraryUnprovenPolyTransfer.arbitrary, ModelGenerators.arbitraryCurve25519SK.arbitrary) { (transfer, sk) =>
      implicit val blake2b256: Blake2b256 = new Blake2b256

      val underTest = SignableInstances.implicits.unprovenTransactionSignable

      val messageToSign = underTest.signableBytes(transfer)

      messageToSign shouldBe expectedMessageToSign(sk, transfer).value
    }
  }

  it should "generate the expected signable bits from an arbit transfer" in {
    forAll(arbitraryUnprovenArbitTransfer.arbitrary, ModelGenerators.arbitraryCurve25519SK.arbitrary) {
      (transfer, sk) =>
        implicit val blake2b256: Blake2b256 = new Blake2b256

        val underTest = SignableInstances.implicits.unprovenTransactionSignable

        val messageToSign = underTest.signableBytes(transfer)

        messageToSign shouldBe expectedMessageToSign(sk, transfer).value
    }
  }

  it should "generate the expected bits from an asset transfer" in {
    forAll(arbitraryUnprovenAssetTransfer.arbitrary, ModelGenerators.arbitraryCurve25519SK.arbitrary) {
      (transfer, sk) =>
        implicit val blake2b256: Blake2b256 = new Blake2b256

        val underTest = SignableInstances.implicits.unprovenTransactionSignable

        val messageToSign = underTest.signableBytes(transfer)

        messageToSign shouldBe expectedMessageToSign(sk, transfer).value
    }
  }

  it should "generate the expected bits from an arbit transfer containing no non-zero outputs" in {
    forAll(
      arbitraryUnprovenArbitTransfer.arbitrary,
      ModelGenerators.arbitraryArbitOutput.arbitrary,
      ModelGenerators.arbitraryCurve25519SK.arbitrary,
      Gen.negNum[Long]
    ) { (arbitTransfer, arbitOutput, sk, arbitOutputValue) =>
      implicit val blake2b256: Blake2b256 = new Blake2b256

      val transfer =
        arbitTransfer.copy(coinOutputs =
          NonEmptyChain.one[Transaction.CoinOutput](arbitOutput.copy(value = Sized.maxUnsafe(arbitOutputValue)))
        )

      val underTest = SignableInstances.implicits.unprovenTransactionSignable

      val messageToSign = underTest.signableBytes(transfer)

      messageToSign shouldBe expectedMessageToSign(sk, transfer).value
    }
  }

  it should "generate the expected bits from an asset transfer containing no non-zero outputs" in {
    forAll(
      arbitraryUnprovenAssetTransfer.arbitrary,
      ModelGenerators.arbitraryAssetOutput.arbitrary,
      ModelGenerators.arbitraryCurve25519SK.arbitrary,
      Gen.negNum[Long]
    ) { (assetTransfer, assetOutput, sk, assetOutputValue) =>
      implicit val blake2b256: Blake2b256 = new Blake2b256

      val transfer =
        assetTransfer.copy(coinOutputs =
          NonEmptyChain.one[Transaction.CoinOutput](
            assetOutput.copy(value =
              assetOutput.value.copy(
                quantity = Sized.maxUnsafe(assetOutputValue),
                assetCode = assetOutput.value.assetCode.copy(version = 1)
              )
            )
          )
        )

      val underTest = SignableInstances.implicits.unprovenTransactionSignable

      val messageToSign = underTest.signableBytes(transfer)

      messageToSign shouldBe expectedMessageToSign(sk, transfer).value
    }
  }
}

object SignableInstancesSpec {

  def expectedMessageToSign(
    sk:       SecretKeys.Curve25519,
    transfer: Transaction.Unproven
  ): Either[TetraTransactionOps.ToDionTxFailure, Bytes] = {
    // need to mock attestation in order to convert from a transfer to a Dion TX
    val proposition = sk.vk.asProposition
    val proof = Curve25519.instance.sign(sk, Bytes("test".getBytes))

    val signedTransfer = transfer.prove(_ => proposition -> proof)
    val dionTransfer = signedTransfer.toDionTx

    dionTransfer.map(_.messageToSign).map(Bytes.apply)
  }

  def arbitraryNonEmptyChain[T](arbitraryT: Arbitrary[T]): Arbitrary[NonEmptyChain[T]] =
    Arbitrary(
      Gen
        .zip(arbitraryT.arbitrary, Gen.listOf(arbitraryT.arbitrary))
        .map(values => NonEmptyChain.one(values._1).appendChain(Chain.fromSeq(values._2)))
    )

  val arbitraryUnprovenPolyTransfer: Arbitrary[Transaction.Unproven] =
    Arbitrary(
      Gen
        .zip(
          ModelGenerators.arbitraryUnprovenTransaction.arbitrary,
          arbitraryNonEmptyChain(ModelGenerators.arbitraryPolyOutput).arbitrary
        )
        .map(values => values._1.copy(coinOutputs = values._2.map[Transaction.CoinOutput](x => x)))
    )

  val arbitraryUnprovenArbitTransfer: Arbitrary[Transaction.Unproven] =
    Arbitrary(
      Gen
        .zip(
          ModelGenerators.arbitraryUnprovenTransaction.arbitrary,
          arbitraryNonEmptyChain(ModelGenerators.arbitraryArbitOutput).arbitrary
        )
        .map(values => values._1.copy(coinOutputs = values._2.map[Transaction.CoinOutput](x => x)))
    )

  val arbitraryUnprovenAssetTransfer: Arbitrary[Transaction.Unproven] =
    Arbitrary(
      Gen
        .zip(
          ModelGenerators.arbitraryUnprovenTransaction.arbitrary,
          ModelGenerators.arbitraryAssetCode.arbitrary,
          arbitraryNonEmptyChain(ModelGenerators.arbitraryAssetOutput).arbitrary
        )
        .map(values =>
          values._1.copy(
            coinOutputs = values._3
              .map(output => output.copy(value = output.value.copy(assetCode = values._2.copy(version = 1.toByte))))
              .map(x => x: Transaction.CoinOutput)
          )
        )
    )
}
