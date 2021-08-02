package co.topl.attestation.keyManagement.derivedKeys

import cats.implicits._
import co.topl.attestation.SignatureEd25519
import co.topl.attestation.keyManagement.derivedKeys.ExtendedPrivateKeyEd25519.InvalidDerivedKey
import co.topl.attestation.keyManagement.mnemonicSeed.Mnemonic.Mnemonic15
import co.topl.attestation.keyManagement.mnemonicSeed.{English, Mnemonic}
import co.topl.crypto.signatures.Ed25519
import co.topl.utils.SizedByteCollection
import co.topl.utils.SizedByteCollection.Types.ByteVector32
import co.topl.utils.SizedByteCollection.implicits._
import co.topl.utils.StringDataTypes.Base16Data
import co.topl.utils.codecs.implicits._
import co.topl.utils.encode.Base16
import org.scalacheck.Gen
import org.scalacheck.Gen.asciiStr
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks.forAll
import scodec.bits.ByteOrdering

// Test Vectors:
// https://github.com/satoshilabs/slips/blob/master/slip-0023.md and
// https://github.com/input-output-hk/rust-cardano/blob/master/cardano/src/hdwallet.rs
class ExtendedPrivateKeyEd25519Spec extends AnyFlatSpec {

  private val positiveIntListGen: Gen[List[Int]] = Gen.listOf(Gen.chooseNum(0, Int.MaxValue))

  private val testVector1Seed =
    Base16Data.unsafe("578d685d20b602683dc5171df411d3e2")

  private val testVector2Seed =
    Base16Data.unsafe("a055b781aac0c9dc1bfb7d803bc8ffd5d4392e506db2e4a5a93f0aba958c5be7")

  "ExtendedPrivateKeyEd25519.fromSeed" should "create correct root left key with test vector #1 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector1Seed.value)
      .leftNumber
      .toString
      .shouldBe("38096432269777187972282727382530464140043628323029465813805073381215192153792")
  }

  it should "create correct root left key with test vector #2 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector2Seed.value)
      .leftNumber
      .toString
      .shouldBe("35870817594148037193235249761081259065186522922583196642112477624627719791504")
  }

  it should "create correct root right key with test vector #1 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector1Seed.value)
      .rightKey
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("4064253ffefc4127489bce1b825a47329010c5afb4d21154ef949ef786204405"))
  }

  it should "create correct root right key with test vector #2 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector2Seed.value)
      .rightKey
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("f9d99bf3cd9c7e12663e8646afa40cb3aecf15d91f2abc15d21056c6bccb3414"))
  }

  it should "create correct root public key with test vector #1 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector1Seed.value)
      .publicKey
      .bytes
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("83e3ecaf57f90f022c45e10d1b8cb78499c30819515ad9a81ad82139fdb12a90"))
  }

  it should "create correct root public key with test vector #2 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector2Seed.value)
      .publicKey
      .bytes
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("eea170f0ef97b59d22907cb429888029721ed67d3e7a1b56b81731086ab7db64"))
  }

  it should "create correct root chain code with test vector #1 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector1Seed.value)
      .chainCode
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("22c12755afdd192742613b3062069390743ea232bc1b366c8f41e37292af9305"))
  }

  it should "create correct root chain code with test vector #2 seed" in {
    ExtendedPrivateKeyEd25519
      .fromSeed(testVector2Seed.value)
      .chainCode
      .encodeAsBase16
      .shouldBe(Base16Data.unsafe("04f1de750b62725fcc1ae1b93ca4063acb53c486b959cadaa100ebd7828e5460"))
  }

  it should "create a valid signing root key with test vector #1" in {
    val root = ExtendedPrivateKeyEd25519.fromSeed(testVector1Seed.value)

    val publicKey = root.publicKey

    val message = "test".getBytes

    val signature = root.sign(message)

    val ec = new Ed25519

    val isValid = ec.verify(signature.sigBytes, message, publicKey.toPublicKey)

    isValid shouldBe true
  }

  it should "create a valid signing root key with test vector #2" in {
    val root = ExtendedPrivateKeyEd25519.fromSeed(testVector2Seed.value)

    val publicKey = root.publicKey

    val message = "test".getBytes

    val signature = root.sign(message)

    val ec = new Ed25519

    val isValid = ec.verify(signature.sigBytes, message, publicKey.toPublicKey)

    isValid shouldBe true
  }

  "ExtendedPrivateKeyEd25519.deriveChildKey" should "generate valid signing keys for a hardened path" in {
    forAll(asciiStr, asciiStr, positiveIntListGen) { (seed, message, path) =>
      val root = ExtendedPrivateKeyEd25519.fromSeed(seed.getBytes)

      val derivedKey = path.foldLeft(root.asRight[InvalidDerivedKey]) {
        case (Right(key), step) => key.deriveChildKey(DerivedKeyIndex.hardened(step))
        case (error, _)         => error
      }

      // do not test invalid keys
      derivedKey.foreach { privateKey =>
        val publicKey = privateKey.publicKey

        val messageToSign = message.getBytes

        val ed25519 = new Ed25519

        val signature = privateKey.sign(messageToSign)

        val isValidSignature = ed25519.verify(signature.sigBytes, messageToSign, publicKey.toPublicKey)

        isValidSignature shouldBe true
      }
    }
  }

  "ExtendedPrivateKeyEd25519" should "pass child key derivation and signing test vector from rust-cardano" in {
    val rootKeyBytes: Array[Byte] =
      Array(
        0xf8, 0xa2, 0x92, 0x31, 0xee, 0x38, 0xd6, 0xc5, 0xbf, 0x71, 0x5d, 0x5b, 0xac, 0x21, 0xc7, 0x50, 0x57, 0x7a,
        0xa3, 0x79, 0x8b, 0x22, 0xd7, 0x9d, 0x65, 0xbf, 0x97, 0xd6, 0xfa, 0xde, 0xa1, 0x5a, 0xdc, 0xd1, 0xee, 0x1a,
        0xbd, 0xf7, 0x8b, 0xd4, 0xbe, 0x64, 0x73, 0x1a, 0x12, 0xde, 0xb9, 0x4d, 0x36, 0x71, 0x78, 0x41, 0x12, 0xeb,
        0x6f, 0x36, 0x4b, 0x87, 0x18, 0x51, 0xfd, 0x1c, 0x9a, 0x24, 0x73, 0x84, 0xdb, 0x9a, 0xd6, 0x00, 0x3b, 0xbd,
        0x08, 0xb3, 0xb1, 0xdd, 0xc0, 0xd0, 0x7a, 0x59, 0x72, 0x93, 0xff, 0x85, 0xe9, 0x61, 0xbf, 0x25, 0x2b, 0x33,
        0x12, 0x62, 0xed, 0xdf, 0xad, 0x0d
      ).map(_.toByte)

    val rootKey =
      ExtendedPrivateKeyEd25519(
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(0, 32), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(32, 64), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(64, 96), ByteOrdering.LittleEndian),
        Seq()
      )

    val expectedChildKey: Array[Byte] =
      Array(
        0x60, 0xd3, 0x99, 0xda, 0x83, 0xef, 0x80, 0xd8, 0xd4, 0xf8, 0xd2, 0x23, 0x23, 0x9e, 0xfd, 0xc2, 0xb8, 0xfe,
        0xf3, 0x87, 0xe1, 0xb5, 0x21, 0x91, 0x37, 0xff, 0xb4, 0xe8, 0xfb, 0xde, 0xa1, 0x5a, 0xdc, 0x93, 0x66, 0xb7,
        0xd0, 0x03, 0xaf, 0x37, 0xc1, 0x13, 0x96, 0xde, 0x9a, 0x83, 0x73, 0x4e, 0x30, 0xe0, 0x5e, 0x85, 0x1e, 0xfa,
        0x32, 0x74, 0x5c, 0x9c, 0xd7, 0xb4, 0x27, 0x12, 0xc8, 0x90, 0x60, 0x87, 0x63, 0x77, 0x0e, 0xdd, 0xf7, 0x72,
        0x48, 0xab, 0x65, 0x29, 0x84, 0xb2, 0x1b, 0x84, 0x97, 0x60, 0xd1, 0xda, 0x74, 0xa6, 0xf5, 0xbd, 0x63, 0x3c,
        0xe4, 0x1a, 0xdc, 0xee, 0xf0, 0x7a
      ).map(_.toByte)

    val expectedSignature: Array[Byte] =
      Array(
        0x90, 0x19, 0x4d, 0x57, 0xcd, 0xe4, 0xfd, 0xad, 0xd0, 0x1e, 0xb7, 0xcf, 0x16, 0x17, 0x80, 0xc2, 0x77, 0xe1,
        0x29, 0xfc, 0x71, 0x35, 0xb9, 0x77, 0x79, 0xa3, 0x26, 0x88, 0x37, 0xe4, 0xcd, 0x2e, 0x94, 0x44, 0xb9, 0xbb,
        0x91, 0xc0, 0xe8, 0x4d, 0x23, 0xbb, 0xa8, 0x70, 0xdf, 0x3c, 0x4b, 0xda, 0x91, 0xa1, 0x10, 0xef, 0x73, 0x56,
        0x38, 0xfa, 0x7a, 0x34, 0xea, 0x20, 0x46, 0xd4, 0xbe, 0x04
      ).map(_.toByte)

    val childKey =
      rootKey
        .deriveChildKey(DerivedKeyIndex.hardened(0))
        .getOrElse(throw new Exception("invalid child key"))

    val childKeyBytes =
      childKey.leftKey.value.toArray ++ childKey.rightKey.value.toArray ++ childKey.chainCode.value.toArray

    val signatureResult: SignatureEd25519 = childKey.sign("Hello World".getBytes("UTF-8"))

    childKeyBytes shouldBe expectedChildKey
    signatureResult.sigBytes shouldBe expectedSignature
  }

  it should "pass generated test vector #1" in {
    val `root private key` =
      "f0d0f18e6ab029166fe4e89519ab64f42aa870fc2791fc472840c3a1ba507347fee30dcae1ae3941bde71e9ddd19eef33d0a7b91" +
        "aaa4137cea6ef4ea3c27f96a1189e5ec0628974ed7846b594ed0ee2d3ef2d8f5b91d1860ffb0a065159df8be"

    val `expected m/'0 private key` =
      "b859fdcdafa6a4552e5d4a18c44b79daf1d40f1600f6745768ddcbd9bc507347b7b1cdaf0d837051ed203813f7f3c518ae8046fbd4" +
        "de106bf1cde33496825a390f2f8270d4724314a2a4f7175cd5765c35dffbf5ccbbfc4f8497297e9e68510f"

    val `expected m/'0 public key` =
      "b983b958d41fbdfecf6c0010ac667efa3cecb02ba27099afd13bc0ef0f82e60c0f2f8270d4724314a2a4f7175cd5765c35df" +
        "fbf5ccbbfc4f8497297e9e68510f"

    val `expected m/'0/'100 private key` =
      "30c9ae886a00e5524223d96824b28b1aff0419c6026dd07509e5b5a4c15073473890a9decc12d0400869d6daf095092863bba4" +
        "5363b8e33c257e70bf7d3548aacce7b986e25839573c044c389cf8f76d8adcc6f723df9f98bfa1308f0c35282c"

    val `expected m/'0/'100 public key` =
      "4b95248060cc3bd0fee38cddf2c54b5e155a38de5cfe1846873355b35cc07566cce7b986e25839573c044c389cf8f76d8adcc6f723" +
        "df9f98bfa1308f0c35282c"

    val `expected m/'0/'100/55 public key` =
      "8e59beac508fcd431c0b7b2dae81686adf45c76c0e32af7af779ecdf78adb8fb3a5c3099aeffe333f39d4107b1f59227a7e5713b9451" +
        "8033a763a542ea289ee8"

    val rootKeyBytes = Base16.decode(`root private key`).getOrElse(throw new Error())

    val rootKey =
      ExtendedPrivateKeyEd25519(
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(0, 32), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(32, 64), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(64, 96), ByteOrdering.LittleEndian),
        Seq()
      )

    val `actual m/'0 private key` =
      rootKey.deriveChildKey(DerivedKeyIndex.hardened(0)).getOrElse(throw new Error())

    Base16.encode((
        `actual m/'0 private key`.leftKey.toVector ++
          `actual m/'0 private key`.rightKey.toVector ++
          `actual m/'0 private key`.chainCode.toVector
    ).toArray) shouldBe `expected m/'0 private key`


    val `actual m/'0 public key` =
      `actual m/'0 private key`.publicKey

    Base16.encode((
      `actual m/'0 public key`.bytes.toVector ++
        `actual m/'0 public key`.chainCode.toVector
      ).toArray) shouldBe `expected m/'0 public key`

    val `actual m/'0/'100 private key` =
      `actual m/'0 private key`.deriveChildKey(DerivedKeyIndex.hardened(100)).getOrElse(throw new Error())

    Base16.encode((
      `actual m/'0/'100 private key`.leftKey.toVector ++
        `actual m/'0/'100 private key`.rightKey.toVector ++
        `actual m/'0/'100 private key`.chainCode.toVector
      ).toArray) shouldBe `expected m/'0/'100 private key`

    val `actual m/'0/'100 public key` =
      `actual m/'0/'100 private key`.publicKey

    Base16.encode((
      `actual m/'0/'100 public key`.bytes.toVector ++
        `actual m/'0/'100 public key`.chainCode.toVector
      ).toArray) shouldBe `expected m/'0/'100 public key`

    val `actual m/'0/'100/55 public key` =
      `actual m/'0/'100 public key`.derive(DerivedKeyIndex.soft(55))

    Base16.encode((
      `actual m/'0/'100/55 public key`.bytes.toVector ++
        `actual m/'0/'100/55 public key`.chainCode.toVector
      ).toArray) shouldBe `expected m/'0/'100/55 public key`
  }

  it should "pass generated test vector #2" in {
    val `root private key` =
      "2090d5cdd6bdc4537ed44f109c261f3f8dbe9c17a843a77c035f55c78a723a481c285eee9cf920be4a1e1e3564763ad100fe203b5fd7" +
        "9f6535943170e53597add20dd0bcf02446e2f607419163f9dbf572393b9c2258d33df59fb0e06112d285"

    val `expected m/'1852/'7091/'0/'0 private key` =
      "60befd4438750e301c86713f2c1a5178d419ff9434d9d3dcf44b9ea5a1723a48a14867f43dc37a11f4b82c10b5c1e7c6b5cc91bcd8c0" +
        "29d180f0aca62dee72f92f5d057d61cce1664344538c61c12d99f74a8a6c331a811d8ecb468b36168ef0"

    val `expected m/'1852/'7091/'0/'0/0 public key` =
      "f119694710657f95edf110002ad3974db4c22f330b6b091355cd0b5784f04ba8b415521a3550f1e59fad614aa249aa" +
        "3245c93005efd63faf8a02ba7787176782"

    val rootKeyBytes = Base16.decode(`root private key`).getOrElse(throw new Error())

    val rootKey =
      ExtendedPrivateKeyEd25519(
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(0, 32), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(32, 64), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(rootKeyBytes.slice(64, 96), ByteOrdering.LittleEndian),
        Seq()
      )

    val `actual m/'1852/'7091/'0/'0 private key` =
      rootKey
        .deriveChildKey(DerivedKeyIndex.hardened(1852)).getOrElse(throw new Error())
        .deriveChildKey(DerivedKeyIndex.hardened(7091)).getOrElse(throw new Error())
        .deriveChildKey(DerivedKeyIndex.hardened(0)).getOrElse(throw new Error())
        .deriveChildKey(DerivedKeyIndex.hardened(0)).getOrElse(throw new Error())

    Base16.encode((
      `actual m/'1852/'7091/'0/'0 private key`.leftKey.toVector ++
        `actual m/'1852/'7091/'0/'0 private key`.rightKey.toVector ++
        `actual m/'1852/'7091/'0/'0 private key`.chainCode.toVector
      ).toArray) shouldBe `expected m/'1852/'7091/'0/'0 private key`


    val `actual m/'1852/'7091/'0/'0/0 public key` =
      `actual m/'1852/'7091/'0/'0 private key`.publicKey.derive(DerivedKeyIndex.soft(0))

    Base16.encode((
      `actual m/'1852/'7091/'0/'0/0 public key`.bytes.toVector ++
        `actual m/'1852/'7091/'0/'0/0 public key`.chainCode.toVector
      ).toArray) shouldBe `expected m/'1852/'7091/'0/'0/0 public key`
  }
}
