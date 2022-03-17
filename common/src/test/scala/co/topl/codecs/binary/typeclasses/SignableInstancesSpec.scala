package co.topl.codecs.binary.typeclasses

import cats.data.{Chain, NonEmptyChain}
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.Curve25519
import co.topl.models.{Bytes, ModelGenerators, Transaction}
import co.topl.modifier.ops.implicits._
import co.topl.typeclasses.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class SignableInstancesSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks with EitherValues {

  import SignableInstancesSpec._

  behavior of "SignableInstances.Instances.unprovenTransactionSignable"

  it should "generate signable bits from a poly transfer that matches a dion transfer message to sign" in {
    forAll(arbitraryUnprovenPolyTransfer.arbitrary, ModelGenerators.arbitraryCurve25519SK.arbitrary) { (transfer, sk) =>
      println(transfer)

      implicit val blake2b256: Blake2b256 = new Blake2b256

      val underTest = SignableInstances.implicits.unprovenTransactionSignable

      val messageToSign = underTest.signableBytes(transfer)

      // need to prove transfer in order to safely convert it to a Dion TX
      val proposition = sk.vk.asProposition
      val proof = Curve25519.instance.sign(sk, messageToSign)

      val signedTransfer = transfer.prove(_ => proposition -> proof)
      val dionTransfer = signedTransfer.toDionTx.value
      println(dionTransfer)
      val expectedMessageToSign = Bytes(signedTransfer.toDionTx.value.messageToSign)

      messageToSign shouldBe expectedMessageToSign
    }
  }

  it should "generate signable bits from an arbit transfer that matches a dion transfer message to sign" in {
    forAll(arbitraryUnprovenArbitTransfer.arbitrary, ModelGenerators.arbitraryCurve25519SK.arbitrary) {
      (transfer, sk) =>
        println(transfer)

        implicit val blake2b256: Blake2b256 = new Blake2b256

        val underTest = SignableInstances.implicits.unprovenTransactionSignable

        val messageToSign = underTest.signableBytes(transfer)

        // need to prove transfer in order to safely convert it to a Dion TX
        val proposition = sk.vk.asProposition
        val proof = Curve25519.instance.sign(sk, messageToSign)

        val signedTransfer = transfer.prove(_ => proposition -> proof)
        val dionTransfer = signedTransfer.toDionTx.value
        println(dionTransfer)
        val expectedMessageToSign = Bytes(signedTransfer.toDionTx.value.messageToSign)

        messageToSign shouldBe expectedMessageToSign
    }
  }

  it should "generate signable bits from an asset transfer that matches a dion transfer message to sign" in {
    forAll(arbitraryUnprovenAssetTransfer.arbitrary, ModelGenerators.arbitraryCurve25519SK.arbitrary) {
      (transfer, sk) =>
        println(transfer)

        implicit val blake2b256: Blake2b256 = new Blake2b256

        val underTest = SignableInstances.implicits.unprovenTransactionSignable

        val messageToSign = underTest.signableBytes(transfer)

        // need to prove transfer in order to safely convert it to a Dion TX
        val proposition = sk.vk.asProposition
        val proof = Curve25519.instance.sign(sk, messageToSign)

        val signedTransfer = transfer.prove(_ => proposition -> proof)
        val dionTransfer = signedTransfer.toDionTx.value
        println(dionTransfer)
        val expectedMessageToSign = Bytes(signedTransfer.toDionTx.value.messageToSign)

        messageToSign shouldBe expectedMessageToSign
    }
  }
}

object SignableInstancesSpec {

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
