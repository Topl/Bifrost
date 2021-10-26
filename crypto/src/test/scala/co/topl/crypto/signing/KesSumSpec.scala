package co.topl.crypto.signing

import co.topl.crypto.utils.Generators._
import co.topl.crypto.utils.Hex.implicits._
import co.topl.models.{Bytes, Proofs, VerificationKeys}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class KesSumSpec extends AnyFlatSpec with ScalaCheckPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {

  "KesSum" should "verify a message signed with the appropriate public key" in {
    forAll(
      genRandomlySizedBytes,
      genRandomlySizedBytes,
      genRandomlySizedBytes,
      genRandomlySizedBytes,
      Gen.choose(1, 12)
    ) { (seed1: Bytes, seed2: Bytes, message1: Bytes, message2: Bytes, height: Int) =>
      whenever(!(seed1 == seed2) && !(message1 == message2)) {
        val kesSum = new KesSum
        val (sk1, vk1) = kesSum.createKeyPair(seed1, height, 0)
        val (_, vk2) = kesSum.createKeyPair(seed2, height, 0)
        val sig = kesSum.sign(sk1, message1)

        kesSum.verify(sig, message1, vk1) shouldBe true
        kesSum.verify(sig, message1, vk2) shouldBe false
        kesSum.verify(sig, message2, vk1) shouldBe false
      }
    }
  }
  it should "generate identical keypairs given the same seed" in {
    forAll(genBytesWithBoundedSize(0, 1024), Gen.choose(1, 12)) { (seedBytes: Bytes, height: Int) =>
      val kesSum = new KesSum
      val (_, vk1) = kesSum.createKeyPair(seedBytes, height, 0)
      val (_, vk2) = kesSum.createKeyPair(seedBytes, height, 0)

      vk1 shouldBe vk2
    }
  }
  it should "test vector - 1 - Generate and verify a specified signature at `t=0`" in {
    val kesSum = new KesSum()
    val specIn_seed = "928b20366943e2afd11ebc0eae2e53a93bf177a4fcf35bcc64d503704e65e202".hexStringToBytes
    val specIn_height = 7
    val specIn_time = 0
    val specIn_msg = "6d657373616765".hexStringToBytes

    val specOut_vk =
      VerificationKeys.KesSum(
        "23d72240b54a6135ec7ca96013d2e4edacefc2ccad2aac861430eeb9286b4ae6".unsafeStrictBytes,
        specIn_time
      )
    val specOut_sig = Proofs.Signature.KesSum(
      VerificationKeys.Ed25519("fce14b4bb0ccc19079263a8115a13fb9cf015c76f16abccb9b5ce91a9e866e6d".unsafeStrictBytes),
      Proofs.Signature.Ed25519(
        "90107bbc5a7d6fd9747f6076e107ce2243d635bfa5439af2464346517405420116c5135c5fa10c9468086fe8d2998a3a04e39efe5971868e4f95fe127ac81709".unsafeStrictBytes
      ),
      Vector(
        "92897adc1a78d0225cd2a3ec84b94b84ac2cf175b9809aeeebea18f9958552f0".unsafeStrictBytes,
        "64142027adac58b88711a61e4ca6675d16a02755fb87871d8b676ecbe829fc8a".unsafeStrictBytes,
        "1c62dc579ea9b9aae5ca65998256f5a702c9cea2125f24927efd92ee97777705".unsafeStrictBytes,
        "490975d0f3d5a6a99e1302a2adbff0604441f13c6b13f6d99e45d51532ba0dae".unsafeStrictBytes,
        "8be89df4800275fcaf3426ce8f3b0318cc19a23274474e74e56c6d38d6ad03a0".unsafeStrictBytes,
        "8592a55902f5f3ab6d2247585c5472c1d9f2132481a3d5cc39a7ff5edf435491".unsafeStrictBytes,
        "9f2ed5b9d1df27926d225f8eb41c426254d4218e054f5c4a102eeccb58876596".unsafeStrictBytes
      )
    )

    val (sk, vk) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sig = kesSum.sign(sk, specIn_msg)

    vk shouldBe specOut_vk
    sig shouldBe specOut_sig
    kesSum.verify(sig, specIn_msg, vk) shouldBe true
  }
  it should "test vector - 2 - Generate and verify a specified signature at `t=1`" in {
    val kesSum = new KesSum()
    val specIn_seed = "928b20366943e2afd11ebc0eae2e53a93bf177a4fcf35bcc64d503704e65e202".hexStringToBytes
    val specIn_height = 7
    val specIn_time = 1
    val specIn_msg = "6d657373616765".hexStringToBytes

    val specOut_vk =
      VerificationKeys.KesSum(
        "23d72240b54a6135ec7ca96013d2e4edacefc2ccad2aac861430eeb9286b4ae6".unsafeStrictBytes,
        specIn_time
      )
    val specOut_sig = Proofs.Signature.KesSum(
      VerificationKeys.Ed25519("8ad3faaeac74cd22846f2eaefa87d41f520a804c1a8c92d7eca338634e652fbd".unsafeStrictBytes),
      Proofs.Signature.Ed25519(
        "d634d8527d79f7ee79ee1f1dfba3aa4da524b45679227ff948304820bc608409adc1444d9f369b3c1afbb720d27a32397212273f77e7f427570d4ce8a935fe0e".unsafeStrictBytes
      ),
      Vector(
        "955ce11603b1ed207b11904e7bec4b5ca1985a64e2e18fd3d109854403f7bec0".unsafeStrictBytes,
        "64142027adac58b88711a61e4ca6675d16a02755fb87871d8b676ecbe829fc8a".unsafeStrictBytes,
        "1c62dc579ea9b9aae5ca65998256f5a702c9cea2125f24927efd92ee97777705".unsafeStrictBytes,
        "490975d0f3d5a6a99e1302a2adbff0604441f13c6b13f6d99e45d51532ba0dae".unsafeStrictBytes,
        "8be89df4800275fcaf3426ce8f3b0318cc19a23274474e74e56c6d38d6ad03a0".unsafeStrictBytes,
        "8592a55902f5f3ab6d2247585c5472c1d9f2132481a3d5cc39a7ff5edf435491".unsafeStrictBytes,
        "9f2ed5b9d1df27926d225f8eb41c426254d4218e054f5c4a102eeccb58876596".unsafeStrictBytes
      )
    )

    val (sk0, _) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sk = kesSum.update(sk0, specIn_time)
    val vk = kesSum.getVerificationKey(sk)
    val sig = kesSum.sign(sk, specIn_msg)

    vk shouldBe specOut_vk
    sig shouldBe specOut_sig
    kesSum.verify(sig, specIn_msg, vk) shouldBe true
  }
  it should "test vector - 3 - Generate and verify a specified signature at `t=10`" in {
    val kesSum = new KesSum()
    val specIn_seed = "928b20366943e2afd11ebc0eae2e53a93bf177a4fcf35bcc64d503704e65e202".hexStringToBytes
    val specIn_height = 7
    val specIn_time = 10
    val specIn_msg = "6d657373616765".hexStringToBytes

    val specOut_vk =
      VerificationKeys.KesSum(
        "23d72240b54a6135ec7ca96013d2e4edacefc2ccad2aac861430eeb9286b4ae6".unsafeStrictBytes,
        specIn_time
      )
    val specOut_sig = Proofs.Signature.KesSum(
      VerificationKeys.Ed25519("8ad3faaeac74cd22846f2eaefa87d41f520a804c1a8c92d7eca338634e652fbd".unsafeStrictBytes),
      Proofs.Signature.Ed25519(
        "d634d8527d79f7ee79ee1f1dfba3aa4da524b45679227ff948304820bc608409adc1444d9f369b3c1afbb720d27a32397212273f77e7f427570d4ce8a935fe0e".unsafeStrictBytes
      ),
      Vector(
        "955ce11603b1ed207b11904e7bec4b5ca1985a64e2e18fd3d109854403f7bec0".unsafeStrictBytes,
        "64142027adac58b88711a61e4ca6675d16a02755fb87871d8b676ecbe829fc8a".unsafeStrictBytes,
        "1c62dc579ea9b9aae5ca65998256f5a702c9cea2125f24927efd92ee97777705".unsafeStrictBytes,
        "490975d0f3d5a6a99e1302a2adbff0604441f13c6b13f6d99e45d51532ba0dae".unsafeStrictBytes,
        "8be89df4800275fcaf3426ce8f3b0318cc19a23274474e74e56c6d38d6ad03a0".unsafeStrictBytes,
        "8592a55902f5f3ab6d2247585c5472c1d9f2132481a3d5cc39a7ff5edf435491".unsafeStrictBytes,
        "9f2ed5b9d1df27926d225f8eb41c426254d4218e054f5c4a102eeccb58876596".unsafeStrictBytes
      )
    )

    val (sk0, _) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sk = kesSum.update(sk0, specIn_time)
    val vk = kesSum.getVerificationKey(sk)
    val sig = kesSum.sign(sk, specIn_msg)

    vk shouldBe specOut_vk
    sig shouldBe specOut_sig
    kesSum.verify(sig, specIn_msg, vk) shouldBe true
  }
  it should "test vector - 4 - Generate and verify a specified signature at `t=100`" in {
    val kesSum = new KesSum()
    val specIn_seed = "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes
    val specIn_height = 7
    val specIn_time = 100
    val specIn_msg = "6d657373616765".hexStringToBytes

    val specOut_vk =
      VerificationKeys.KesSum(
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        specIn_time
      )
    val specOut_sig = Proofs.Signature.KesSum(
      VerificationKeys.Ed25519("0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes),
      Proofs.Signature.Ed25519(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes
      ),
      Vector(
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes
      )
    )

    val (sk0, _) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sk = kesSum.update(sk0, specIn_time)
    val vk = kesSum.getVerificationKey(sk)
    val sig = kesSum.sign(sk, specIn_msg)

    vk shouldBe specOut_vk
    sig shouldBe specOut_sig
    kesSum.verify(sig, specIn_msg, vk) shouldBe true
  }
  it should "test vector - 5 - Generate and verify a specified signature at `t=0`" in {
    val kesSum = new KesSum()
    val specIn_seed = "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes
    val specIn_height = 2
    val specIn_time = 0
    val specIn_msg = "6d657373616765".hexStringToBytes

    val specOut_vk =
      VerificationKeys.KesSum(
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        specIn_time
      )
    val specOut_sig = Proofs.Signature.KesSum(
      VerificationKeys.Ed25519("0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes),
      Proofs.Signature.Ed25519(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes
      ),
      Vector(
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes
      )
    )

    val (sk, vk) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sig = kesSum.sign(sk, specIn_msg)

    vk shouldBe specOut_vk
    sig shouldBe specOut_sig
    kesSum.verify(sig, specIn_msg, vk) shouldBe true
  }
  it should "test vector - 6 - Generate and verify a specified signature at `t=1`" in {
    val kesSum = new KesSum()
    val specIn_seed = "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes
    val specIn_height = 2
    val specIn_time = 1
    val specIn_msg = "6d657373616765".hexStringToBytes

    val specOut_vk =
      VerificationKeys.KesSum(
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        specIn_time
      )
    val specOut_sig = Proofs.Signature.KesSum(
      VerificationKeys.Ed25519("0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes),
      Proofs.Signature.Ed25519(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes
      ),
      Vector(
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes
      )
    )

    val (sk0, _) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sk = kesSum.update(sk0, specIn_time)
    val vk = kesSum.getVerificationKey(sk)
    val sig = kesSum.sign(sk, specIn_msg)

    vk shouldBe specOut_vk
    sig shouldBe specOut_sig
    kesSum.verify(sig, specIn_msg, vk) shouldBe true
  }
  it should "test vector - 7 - Generate and verify a specified signature at `t=2`" in {
    val kesSum = new KesSum()
    val specIn_seed = "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes
    val specIn_height = 2
    val specIn_time = 2
    val specIn_msg = "6d657373616765".hexStringToBytes

    val specOut_vk =
      VerificationKeys.KesSum(
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        specIn_time
      )
    val specOut_sig = Proofs.Signature.KesSum(
      VerificationKeys.Ed25519("0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes),
      Proofs.Signature.Ed25519(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes
      ),
      Vector(
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes
      )
    )

    val (sk0, _) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sk = kesSum.update(sk0, specIn_time)
    val vk = kesSum.getVerificationKey(sk)
    val sig = kesSum.sign(sk, specIn_msg)

    vk shouldBe specOut_vk
    sig shouldBe specOut_sig
    kesSum.verify(sig, specIn_msg, vk) shouldBe true
  }
  it should "test vector - 8 - Generate and verify a specified signature at `t=3`" in {
    val kesSum = new KesSum()
    val specIn_seed = "0000000000000000000000000000000000000000000000000000000000000000".hexStringToBytes
    val specIn_height = 2
    val specIn_time = 3
    val specIn_msg = "6d657373616765".hexStringToBytes

    val specOut_vk =
      VerificationKeys.KesSum(
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        specIn_time
      )
    val specOut_sig = Proofs.Signature.KesSum(
      VerificationKeys.Ed25519("0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes),
      Proofs.Signature.Ed25519(
        "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes
      ),
      Vector(
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes,
        "0000000000000000000000000000000000000000000000000000000000000000".unsafeStrictBytes
      )
    )

    val (sk0, _) = kesSum.createKeyPair(specIn_seed, specIn_height, 0)
    val sk = kesSum.update(sk0, specIn_time)
    val vk = kesSum.getVerificationKey(sk)
    val sig = kesSum.sign(sk, specIn_msg)

    vk shouldBe specOut_vk
    sig shouldBe specOut_sig
    kesSum.verify(sig, specIn_msg, vk) shouldBe true
  }

}
