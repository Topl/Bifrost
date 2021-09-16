package co.topl.crypto

import org.scalatest.propspec.AnyPropSpec
import co.topl.crypto.utils.Hex
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Pbkdf2Sha512Spec extends AnyPropSpec {

  property("should pass all test vectors") {
    case class Pbkdf2Sha512TestVector(password: String, salt: String, keySize: Int, iterations: Int, result: String)

    // https://stackoverflow.com/questions/15593184/pbkdf2-hmac-sha-512-test-vectors
    val testVectors = Seq(
      Pbkdf2Sha512TestVector(
        "password",
        "salt",
        64,
        1,
        "86 7f 70 cf 1a de 02 cf f3 75 25 99 a3 a5 3d c4 af 34 c7 a6 69 81 5a e5 d5 13 55 4e 1c 8c f2 52 c0 2d 47 0a " +
        "28 5a 05 01 ba d9 99 bf e9 43 c0 8f 05 02 35 d7 d6 8b 1d a5 5e 63 f7 3b 60 a5 7f ce"
      ),
      Pbkdf2Sha512TestVector(
        "password",
        "salt",
        64,
        2,
        "e1 d9 c1 6a a6 81 70 8a 45 f5 c7 c4 e2 15 ce b6 6e 01 1a 2e 9f 00 40 71 3f 18 ae fd b8 66 d5 3c f7 6c ab 28 " +
        "68 a3 9b 9f 78 40 ed ce 4f ef 5a 82 be 67 33 5c 77 a6 06 8e 04 11 27 54 f2 7c cf 4e "
      ),
      Pbkdf2Sha512TestVector(
        "password",
        "salt",
        64,
        4096,
        "d1 97 b1 b3 3d b0 14 3e 01 8b 12 f3 d1 d1 47 9e 6c de bd cc 97 c5 c0 f8 7f 69 02 e0 72 f4 57 b5 14 3f 30 60 " +
        "26 41 b3 d5 5c d3 35 98 8c b3 6b 84 37 60 60 ec d5 32 e0 39 b7 42 a2 39 43 4a f2 d5 "
      ),
      Pbkdf2Sha512TestVector(
        "passwordPASSWORDpassword",
        "saltSALTsaltSALTsaltSALTsaltSALTsalt",
        64,
        4096,
        "8c 05 11 f4 c6 e5 97 c6 ac 63 15 d8 f0 36 2e 22 5f 3c 50 14 95 ba 23 b8 68 c0 05 17 4d c4 ee 71 11 5b 59 f9 " +
        "e6 0c d9 53 2f a3 3e 0f 75 ae fe 30 22 5c 58 3a 18 6c d8 2b d4 da ea 97 24 a3 d3 b8 "
      )
    )

    testVectors.foreach { vector =>
      val expectedResult = Hex.encode(bytesStringToArray(vector.result))

      val result = Pbkdf2Sha512.generateKey(
        vector.password.getBytes("UTF-8"),
        vector.salt.getBytes("UTF-8"),
        vector.keySize,
        vector.iterations
      )

      val hexResult = Hex.encode(result)

      hexResult shouldBe expectedResult
    }
  }

  def bytesStringToArray(bytes: String): Array[Byte] = {
    val result = bytes.split(" ").flatMap(Hex.decode)

    result
  }
}
