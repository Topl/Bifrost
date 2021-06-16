package co.topl.attestation.keyManagement.wallet

import co.topl.utils.encode.Base58
import co.topl.utils.codecs.implicits._
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import scodec.bits._

import java.io.ByteArrayOutputStream

// SLIP 0023 TestVectors are from https://github.com/satoshilabs/slips/blob/master/slip-0023.md
class WalletPackageSpec extends AnyFlatSpec {

  def toAddress(publicKey: ExtendedPublicKey): String =
    "xpub" + Base58.encode(publicKey.publickeybytes.toArray)

  def toAddress(privateKey: ExtendedPrivateKey): String = {
    val out = new ByteArrayOutputStream()
    out.write(privateKey.depth)
    out.write(privateKey.parent.toInt)
    out.write(privateKey.path.lastChildNumber.toInt)
    out.writeBytes(privateKey.chaincode.bytes.toArray)
    out.write(0)
    out.writeBytes(privateKey.secretKeyBytesRight.bytes.toArray)
    val buffer = ByteVector.view(out.toByteArray)
    "xprv" + Base58.encode(buffer.toArray)
  }

  def withTestVectorSeed(seed: String, number: Int, tests: Seq[DerivedPathTest]): Unit =
    tests.foreach(t => t(seed, number))

  def testVector1Tests(tests: DerivedPathTest*): Unit =
    withTestVectorSeed("578d685d20b602683dc5171df411d3e2", 1, tests)

  def testVector2Tests(tests: DerivedPathTest*): Unit =
    withTestVectorSeed(
      "fffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542",
      2,
      tests
    )

  def testVector3Tests(tests: DerivedPathTest*): Unit = withTestVectorSeed(
    "4b381541583be4423346c643850da4b320e46a87ae3d2a4e6da11eba819cd4acba45d239319ac14f863b8d5ab5a0d0c64d2e8a1e7d1457df2e5a3c51c73235be",
    3,
    tests
  )

  type DerivedPathTest = (String, Int) => Unit

  def path(p: Long*): DerivedPrivatePath = DerivedPrivatePath(p)

  case class DerivedPrivatePath(privatePath: Seq[Long]) {

    private def privateKey(seed: String): ExtendedPrivateKey =
      privatePath.foldLeft(generate(ByteVector.fromValidHex(seed)))((k, idx) => derivePrivateKey(k, idx))

    private def testWithSeed(test: ExtendedPrivateKey => Assertion, name: String)(seed: String, number: Int): Unit = {
      val pk = privateKey(seed)
      val pathStr = pk.path.toString

      it should s"generate correct $name for path $pathStr (Test Vector #$number)" in {
        test(pk)
      }
    }

    def hasSecretKey(expected: String): DerivedPathTest =
      testWithSeed(_.sk.toString shouldBe expected, "secret key")

    def hasSecretKeyBytes(expected: String): DerivedPathTest =
      testWithSeed(publicKey(_).pkHex shouldBe expected, "secret key bytes")

    def hasPublicKey(expected: String): DerivedPathTest =
      testWithSeed(publicKey(_).pkHex shouldBe expected, "public key")

    def hasChainCode(expected: String): DerivedPathTest =
      testWithSeed(_.chaincode.bytes.toHex shouldBe expected, "chain code")

    def hasAddress(expected: String): DerivedPathTest =
      testWithSeed(toAddress(_) shouldBe expected, "secret key address")

    def withPublicPath(path: Long*): DerivedPublicPath = DerivedPublicPath(privateKey, path)
  }

  case class DerivedPublicPath(privateKeyParent: String => ExtendedPrivateKey, path: Seq[Long]) {

    private def testWithSeed(test: ExtendedPublicKey => Assertion, name: String)(seed: String, number: Int): Unit = {
      val rootSk = privateKeyParent(seed)

      val pubKey = path.foldLeft(publicKey(rootSk))(derivePublicKey(_, _, rootSk))

      val pathStr = pubKey.path.toString
      val rootSkPathStr = rootSk.path

      it should s"generate correct $name for path $rootSkPathStr / Pub $pathStr (Test Vector #$number)" in {
        test(pubKey)
      }
    }

    def hasAddress(expected: String): DerivedPathTest = testWithSeed(toAddress(_) shouldBe expected, "public key address")
  }

  testVector1Tests(
    path() hasSecretKey "38096432269777187972282727382530464140043628323029465813805073381215192153792",
    path() hasPublicKey "83e3ecaf57f90f022c45e10d1b8cb78499c30819515ad9a81ad82139fdb12a90",
    path() hasChainCode "22c12755afdd192742613b3062069390743ea232bc1b366c8f41e37292af9305",
    path() hasSecretKeyBytes "4064253ffefc4127489bce1b825a47329010c5afb4d21154ef949ef786204405",
    path(hardened(0)) hasSecretKey "68e0fe46dfb67e368c75379acec591dad19df3cde26e63b93a8e704f1dade7a3",
    path(hardened(0)) hasPublicKey "008c8a13df77a28f3445213a0f432fde644acaa215fc72dcdf300d5efaa85d350c",
    path(hardened(0), 1) hasSecretKey "008c8a13df77a28f3445213a0f432fde644acaa215fc72dcdf300d5efaa85d350c",
    path(hardened(0), 1, hardened(2))
      .hasSecretKey("cbce0d719ecf7431d88e6a89fa1483e02e35092af60c042b1df2ff59fa424dca"),
    path(hardened(0), 1, hardened(2), 2)
      .hasSecretKey("0f479245fb19a38a1954c5c7c0ebab2f9bdfd96a17563ef28a6a4b1a2a764ef4"),
    path(hardened(0), 1, hardened(2), 1000000000)
      .hasSecretKey("471b76e389e528d6de6d816857e012c5455051cad6660850e58372a6c3e6e7c8"),
    path(hardened(0), 1)
      .hasAddress(
        "xprv9wTYmMFdV23N2TdNG573QoEsfRrWKQgWeibmLntzniatZvR9BmLnvSxqu53Kw1UmYPxLgboyZQaXwTCg8MSY3H2EU4pWcQDnRnrVA1xe8fs"
      ),
    path(hardened(0), 1)
      .withPublicPath()
      .hasAddress(
        "xpub6ASuArnXKPbfEwhqN6e3mwBcDTgzisQN1wXN9BJcM47sSikHjJf3UFHKkNAWbWMiGj7Wf5uMash7SyYq527Hqck2AxYysAA7xmALppuCkwQ"
      ),
    path(hardened(0), 1, hardened(2))
      .hasAddress(
        "xprv9z4pot5VBttmtdRTWfWQmoH1taj2axGVzFqSb8C9xaxKymcFzXBDptWmT7FwuEzG3ryjH4ktypQSAewRiNMjANTtpgP4mLTj34bhnZX7UiM"
      ),
    path(hardened(0), 1, hardened(2))
      .withPublicPath()
      .hasAddress(
        "xpub6D4BDPcP2GT577Vvch3R8wDkScZWzQzMMUm3PWbmWvVJrZwQY4VUNgqFJPMM3No2dFDFGTsxxpG5uJh7n7epu4trkrX7x7DogT5Uv6fcLW5"
      ),
    path(hardened(0), 1, hardened(2), 2)
      .hasAddress(
        "xprvA2JDeKCSNNZky6uBCviVfJSKyQ1mDYahRjijr5idH2WwLsEd4Hsb2Tyh8RfQMuPh7f7RtyzTtdrbdqqsunu5Mm3wDvUAKRHSC34sJ7in334"
      ),
    path(hardened(0), 1, hardened(2), 2)
      .withPublicPath(0)
      .hasAddress(
        "xpub6FHa3pjLCk84BayeJxFW2SP4XRrFd1JYnxeLeU8EqN3vDfZmbqBqaGJAyiLjTAwm6ZLRQUMv1ZACTj37sR62cfN7fe5JnJ7dh8zL4fiyLHV"
      ),
    path(hardened(0), 1, hardened(2), 2)
      .withPublicPath(1)
      .hasAddress(
        "xpub6FHa3pjLCk84BayeJxFW2SP4XRrFd1JYnxeLeU8EqN3vDfZmbqBqaGJAyiLjTAwm6ZLRQUMv1ZACTj37sR62cfN7fe5JnJ7dh8zL4fiyLHV"
      ),
    path(hardened(0), 1, hardened(2), 2, 1000000000)
      .hasAddress(
        "xprvA41z7zogVVwxVSgdKUHDy1SKmdb533PjDz7J6N6mV6uS3ze1ai8FHa8kmHScGpWmj4WggLyQjgPie1rFSruoUihUZREPSL39UNdE3BBDu76"
      ),
    path(hardened(0), 1, hardened(2), 2, 1000000000)
      .withPublicPath()
      .hasAddress(
        "xpub6H1LXWLaKsWFhvm6RVpEL9P4KfRZSW7abD2ttkWP3SSQvnyA8FSVqNTEcYFgJS2UaFcxupHiYkro49S8yGasTvXEYBVPamhGW6cFJodrTHy"
      )
  )

  testVector2Tests(
    path().hasAddress(
      "xprv9s21ZrQH143K31xYSDQpPDxsXRTUcvj2iNHm5NUtrGiGG5e2DtALGdso3pGz6ssrdK4PFmM8NSpSBHNqPqm55Qn3LqFtT2emdEXVYsCzC2U"
    ),
    path()
      .withPublicPath()
      .hasAddress(
        "xpub661MyMwAqRbcFW31YEwpkMuc5THy2PSt5bDMsktWQcFF8syAmRUapSCGu8ED9W6oDMSgv6Zz8idoc4a6mr8BDzTJY47LJhkJ8UB7WEGuduB"
      ),
    path(0).hasAddress(
      "xprv9vHkqa6EV4sPZHYqZznhT2NPtPCjKuDKGY38FBWLvgaDx45zo9WQRUT3dKYnjwih2yJD9mkrocEZXo1ex8G81dwSM1fwqWpWkeS3v86pgKt"
    ),
    path(0)
      .withPublicPath()
      .hasAddress(
        "xpub69H7F5d8KSRgmmdJg2KhpAK8SR3DjMwAdkxj3ZuxV27CprR9LgpeyGmXUbC6wb7ERfvrnKZjXoUmmDznezpbZb7ap6r1D3tgFxHmwMkQTPH"
      ),
    path(0, hardened(2147483647)).hasAddress(
      "xprv9wSp6B7kry3Vj9m1zSnLvN3xH8RdsPP1Mh7fAaR7aRLcQMKTR2vidYEeEg2mUCTAwCd6vnxVrcjfy2kRgVsFawNzmjuHc2YmYRmagcEPdU9"
    ),
    path(0, hardened(2147483647))
      .withPublicPath()
      .hasAddress(
        "xpub69H7F5d8KSRgmmdJg2KhpAK8SR3DjMwAdkxj3ZuxV27CprR9LgpeyGmXUbC6wb7ERfvrnKZjXoUmmDznezpbZb7ap6r1D3tgFxHmwMkQTPH"
      ),
    path(0, hardened(2147483647), 1).hasAddress(
      "xpub6DF8uhdarytz3FWdA8TvFSvvAh8dP3283MY7p2V4SeE2wyWmG5mg5EwVvmdMVCQcoNJxGoWaU9DCWh89LojfZ537wTfunKau47EL2dhHKon"
    ),
    path(0, hardened(2147483647), 1, hardened(2147483646)).hasAddress(
      "xprvA1RpRA33e1JQ7ifknakTFpgNXPmW2YvmhqLQYMmrj4xJXXWYpDPS3xz7iAxn8L39njGVyuoseXzU6rcxFLJ8HFsTjSyQbLYnMpCqE2VbFWc"
    ),
    path(0, hardened(2147483647), 1, hardened(2147483646))
      .withPublicPath()
      .hasAddress(
        "xpub6ERApfZwUNrhLCkDtcHTcxd75RbzS1ed54G1LkBUHQVHQKqhMkhgbmJbZRkrgZw4koxb5JaHWkY4ALHY2grBGRjaDMzQLcgJvLJuZZvRcEL"
      ),
    path(0, hardened(2147483647), 1, hardened(2147483646), 2)
      .hasAddress(
        "xprvA2nrNbFZABcdryreWet9Ea4LvTJcGsqrMzxHx98MMrotbir7yrKCEXw7nadnHM8Dq38EGfSh6dqA9QWTyefMLEcBYJUuekgW4BYPJcr9E7j"
      ),
    path(0, hardened(2147483647), 1, hardened(2147483646), 2)
      .withPublicPath()
      .hasAddress(
        "xpub6FnCn6nSzZAw5Tw7cgR9bi15UV96gLZhjDstkXXxvCLsUXBGXPdSnLFbdpq8p9HmGsApME5hQTZ3emM2rnY5agb9rXpVGyy3bdW6EEgAtqt"
      )
  )

  testVector3Tests(
    path().hasAddress(
      "xprv9s21ZrQH143K25QhxbucbDDuQ4naNntJRi4KUfWT7xo4EKsHt2QJDu7KXp1A3u7Bi1j8ph3EGsZ9Xvz9dGuVrtHHs7pXeTzjuxBrCmmhgC6"
    ),
    path()
      .withPublicPath()
      .hasAddress(
        "xpub661MyMwAqRbcEZVB4dScxMAdx6d4nFc9nvyvH3v4gJL378CSRZiYmhRoP7mBy6gSPSCYk6SzXPTf3ND1cZAceL7SfJ1Z3GC8vBgp2epUt13"
      ),
    path(hardened(0)).hasAddress(
      "xprv9uPDJpEQgRQfDcW7BkF7eTya6RPxXeJCqCJGHuCJ4GiRVLzkTXBAJMu2qaMWPrS7AANYqdq6vcBcBUdJCVVFceUvJFjaPdGZ2y9WACViL4L"
    ),
    path(hardened(0))
      .withPublicPath()
      .hasAddress(
        "xpub68NZiKmJWnxxS6aaHmn81bvJeTESw724CRDs6HbuccFQN9Ku14VQrADWgqbhhTHBaohPX4CjNLf9fq9MYo6oDaPPLPxSb7gwQN3ih19Zm4Y"
      )
  )

//  it should "be possible to go up the private key chain if you have the master pub key and a child private key!!" in {
//    val m = generate(hex"000102030405060708090a0b0c0d0e0f")
//    assert(
//      encode(
//        m,
//        xprv
//      ) === "xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi"
//    )
//    val k = new BigInteger(1, m.secretkeybytes.toArray) // k is our master private key
//    val m_pub = publicKey(m)
//    assert(
//      encode(
//        m_pub,
//        xpub
//      ) === "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8"
//    )
//    assert(fingerprint(m) === 876747070)
//    val m42 = derivePrivateKey(m, 42L)
//    // now we have: the master public key, and a child private key, and we want to climb the tree back up
//    // to the parent private key
//    val I = Crypto.hmac512(m_pub.chaincode, m_pub.publickeybytes ++ writeUInt32(42, ByteOrder.BIG_ENDIAN))
//    val IL = I.take(32)
//    val IR = I.takeRight(32)
//    val guess =
//      new BigInteger(1, m42.secretkeybytes.toArray).subtract(new BigInteger(1, IL.toArray)).mod(Crypto.curve.getN)
//    assert(guess === k)
//  }
//
//  it should "parse string-formatted derivation paths" in {
//    assert(KeyPath("m/44'/0'/0'/0") == KeyPath(hardened(44) :: hardened(0) :: hardened(0) :: 0L :: Nil))
//    assert(KeyPath("/44'/0'/0'/0") == KeyPath(hardened(44) :: hardened(0) :: hardened(0) :: 0L :: Nil))
//    assert(KeyPath("44'/0'/0'/0") == KeyPath(hardened(44) :: hardened(0) :: hardened(0) :: 0L :: Nil))
//    assert(KeyPath("m/44/0'/0'/0") == KeyPath(44L :: hardened(0) :: hardened(0) :: 0L :: Nil))
//    assert(KeyPath("m") == KeyPath.Root)
//    assert(KeyPath("") == KeyPath.Root)
//    val invalidKeyPaths = Seq(
//      "aa/1/2/3",
//      "1/'2/3"
//    )
//    invalidKeyPaths.map { path =>
//      intercept[RuntimeException] {
//        println(KeyPath(path))
//      }
//    }
//  }
//
//  it should "be able to derive private keys" in {
//    val random = new Random()
//    val seed = new Array[Byte](32)
//    for (i <- 0 until 50) {
//      random.nextBytes(seed)
//      val master = DeterministicWallet.generate(ByteVector.view(seed))
//      for (j <- 0 until 50) {
//        val index = random.nextLong()
//        val priv = DeterministicWallet.derivePrivateKey(master, index)
//        val encoded = DeterministicWallet.encode(priv, DeterministicWallet.tprv)
//        val (prefix, decoded) = DeterministicWallet.ExtendedPrivateKey.decode(encoded)
//        assert(prefix == DeterministicWallet.tprv)
//        assert(decoded.chaincode == priv.chaincode && decoded.secretkeybytes == priv.secretkeybytes)
//        val pub = DeterministicWallet.publicKey(priv)
//        val encoded1 = DeterministicWallet.encode(pub, DeterministicWallet.tpub)
//        val (prefix1, decoded1) = DeterministicWallet.ExtendedPublicKey.decode(encoded1)
//        assert(prefix1 == DeterministicWallet.tpub)
//        assert(decoded1.chaincode == pub.chaincode && decoded1.publicKey == pub.publicKey)
//      }
//    }
//  }
}
