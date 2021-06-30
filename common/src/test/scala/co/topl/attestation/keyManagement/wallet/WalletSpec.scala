package co.topl.attestation.keyManagement.wallet

import co.topl.crypto.signatures.Ed25519
import org.scalatest.flatspec.AnyFlatSpec
import org.scalacheck.Gen.asciiStr
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import co.topl.utils.SizedByteVector.implicits._
import co.topl.utils.codecs.implicits._
import co.topl.crypto.PublicKey
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import co.topl.utils.StringDataTypes.Base16Data

// Test Vectors: https://github.com/satoshilabs/slips/blob/master/slip-0023.md
class WalletSpec extends AnyFlatSpec {

  private val positiveIntListGen: Gen[List[Int]] = Gen.listOf(Gen.chooseNum(0, Int.MaxValue))

  private val testVector1Seed =
    Base16Data.unsafe("578d685d20b602683dc5171df411d3e2")

  private val testVector2Seed =
    Base16Data.unsafe("a055b781aac0c9dc1bfb7d803bc8ffd5d4392e506db2e4a5a93f0aba958c5be7")

  "Wallet" should "create correct root left key with test vector #1 seed" in {
    ExtendedPrivateKey
      .fromSeed(testVector1Seed.value)
      .leftNumber
      .toString
      .shouldBe("38096432269777187972282727382530464140043628323029465813805073381215192153792")
  }

  it should "create correct root left key with test vector #2 seed" in {
    ExtendedPrivateKey
      .fromSeed(testVector2Seed.value)
      .leftNumber
      .toString
      .shouldBe("35870817594148037193235249761081259065186522922583196642112477624627719791504")
  }

  it should "create correct root right key with test vector #1 seed" in {
    ExtendedPrivateKey
      .fromSeed(testVector1Seed.value)
      .rightKey
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("4064253ffefc4127489bce1b825a47329010c5afb4d21154ef949ef786204405"))
  }

  it should "create correct root right key with test vector #2 seed" in {
    ExtendedPrivateKey
      .fromSeed(testVector2Seed.value)
      .rightKey
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("f9d99bf3cd9c7e12663e8646afa40cb3aecf15d91f2abc15d21056c6bccb3414"))
  }

  it should "create correct root public key with test vector #1 seed" in {
    ExtendedPrivateKey
      .fromSeed(testVector1Seed.value)
      .publicKey
      .bytes
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("83e3ecaf57f90f022c45e10d1b8cb78499c30819515ad9a81ad82139fdb12a90"))
  }

  it should "create correct root public key with test vector #2 seed" in {
    ExtendedPrivateKey
      .fromSeed(testVector2Seed.value)
      .publicKey
      .bytes
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("eea170f0ef97b59d22907cb429888029721ed67d3e7a1b56b81731086ab7db64"))
  }

  it should "create correct root chain code with test vector #1 seed" in {
    ExtendedPrivateKey
      .fromSeed(testVector1Seed.value)
      .chainCode
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("22c12755afdd192742613b3062069390743ea232bc1b366c8f41e37292af9305"))
  }

  it should "create correct root chain code with test vector #2 seed" in {
    ExtendedPrivateKey
      .fromSeed(testVector2Seed.value)
      .chainCode
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("04f1de750b62725fcc1ae1b93ca4063acb53c486b959cadaa100ebd7828e5460"))
  }

  it should "generate valid signing keys for a hardened path" in {
    forAll(asciiStr, asciiStr, positiveIntListGen) { (seed, message, path) =>
      val root = ExtendedPrivateKey.fromSeed(seed.getBytes)

      val privateKey = path.foldLeft(root)((key, step) => key.derive(DerivedKeyIndex.hardened(step)))
      val publicKey = privateKey.publicKey

      val messageToSign = message.getBytes

      val ed25519 = new Ed25519

      val signature = privateKey.sign(messageToSign)

      val isValidSignature = ed25519.verify(signature.sig, messageToSign, PublicKey(publicKey.bytes.toArray))

      isValidSignature shouldBe true
    }
  }

  it should "generate matching chain codes for all public keys from private keys" in {
    forAll(asciiStr, positiveIntListGen) { (seed, path) =>
      val root = ExtendedPrivateKey.fromSeed(seed.getBytes)

      val privateKey = path.foldLeft(root)((key, step) => key.derive(HardenedIndex(step)))
      val publicKey = privateKey.publicKey

      privateKey.chainCode shouldBe publicKey.chainCode
    }
  }

  it should "generate matching chain codes for all soft-index child public keys derived from private keys" in {
    forAll(asciiStr, positiveIntListGen, Gen.chooseNum(0, Int.MaxValue)) { (seed, path, childIndex) =>
      val root = ExtendedPrivateKey.fromSeed(seed.getBytes)

      val privateKey = path.foldLeft(root)((key, step) => key.derive(HardenedIndex(step)))
      val publicKey = privateKey.publicKey

      val derivedChildIndex = DerivedKeyIndex.soft(childIndex)

      val publicKeyChild = publicKey.derive(derivedChildIndex)
      val privateKeyChild = privateKey.derive(derivedChildIndex)

      privateKeyChild.chainCode shouldBe privateKeyChild.chainCode
    }
  }

  it should "generate valid signing keys for all soft-index child public keys derived from private keys" in {
    forAll(asciiStr, positiveIntListGen, Gen.chooseNum(0, Int.MaxValue), asciiStr) {
      (seed, path, childIndex, message) =>
        val root = ExtendedPrivateKey.fromSeed(seed.getBytes)

        val privateKey = path.foldLeft(root)((key, step) => key.derive(HardenedIndex(step)))
        val publicKey = privateKey.publicKey

        val derivedChildIndex = DerivedKeyIndex.soft(childIndex)

        val publicKeyChild = publicKey.derive(derivedChildIndex)
        val privateKeyChild = privateKey.derive(derivedChildIndex)

        val messageToSign = message.getBytes

        val ed25519 = new Ed25519

        val signature = privateKeyChild.sign(messageToSign)

        val isValidSignature = ed25519.verify(signature.sig, messageToSign, PublicKey(publicKeyChild.bytes.toArray))

        isValidSignature shouldBe true
    }
  }

  it should "test" in {
    val root = ExtendedPrivateKey.fromSeed("test".getBytes)

    val privateKey = root.derive(DerivedKeyIndex.soft(1))
    val publicKey = root.publicKey.derive(DerivedKeyIndex.soft(1))

    privateKey.chainCode shouldBe publicKey.chainCode

    val message = "hi".getBytes

    val ed = new Ed25519

    val signature = privateKey.sign(message)

    val isValid = ed.verify(signature.sig, message, publicKey.toPublicKey)

    isValid shouldBe true
  }

  it should "generate matching chain codes for all hardened-index child public keys derived from private keys" in {
    forAll(asciiStr, positiveIntListGen, Gen.chooseNum(0, Int.MaxValue)) { (seed, path, childIndex) =>
      val root = ExtendedPrivateKey.fromSeed(seed.getBytes)

      val privateKey = path.foldLeft(root)((key, step) => key.derive(HardenedIndex(step)))
      val publicKey = privateKey.publicKey

      val derivedChildIndex = DerivedKeyIndex.hardened(childIndex)

      val publicKeyChild = publicKey.derive(derivedChildIndex, privateKey)
      val privateKeyChild = privateKey.derive(derivedChildIndex)

      privateKeyChild.chainCode shouldBe publicKeyChild.chainCode
    }
  }

  it should "create a valid signing root key with test vector #1" in {
    val root = ExtendedPrivateKey.fromSeed(testVector1Seed.value)

    val publicKey = PublicKey(root.publicKey.bytes.toArray)

    val message = "test".getBytes

    val signature = root.sign(message)

    val ec = new Ed25519

    val isValid = ec.verify(signature.sig, message, publicKey)

    isValid shouldBe true
  }

  it should "create a valid signing root key with test vector #2" in {
    val root = ExtendedPrivateKey.fromSeed(testVector2Seed.value)

    val publicKey = PublicKey(root.publicKey.bytes.toArray)

    val message = "test".getBytes

    val signature = root.sign(message)

    val ec = new Ed25519

    val isValid = ec.verify(signature.sig, message, publicKey)

    isValid shouldBe true
  }
}
