package co.topl.client.credential

import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signing.Password
import co.topl.models.{Bytes, DionAddress}
import io.circe.Codec
import cats.implicits._
import io.circe.generic.semiauto.deriveCodec
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.generators.SCrypt
import org.bouncycastle.crypto.modes.SICBlockCipher
import org.bouncycastle.crypto.params.{KeyParameter, ParametersWithIV}

import java.nio.charset.StandardCharsets

// TODO: `evidenceContent` and `keyFileType`
case class KeyFile(address: String, crypto: KeyFile.Crypto)

object KeyFile {

  implicit val cryptoCipherParamsCodec: Codec[Crypto.CipherParams] = deriveCodec
  implicit val cryptokdfParamsCodec: Codec[Crypto.KdfParams] = deriveCodec
  implicit val cryptoCodec: Codec[Crypto] = deriveCodec
  implicit val keyFileCodec: Codec[KeyFile] = deriveCodec

  case class Crypto(
    cipher:       String,
    cipherparams: Crypto.CipherParams,
    ciphertext:   String,
    kdf:          String,
    kdfparams:    Crypto.KdfParams,
    mac:          String
  )

  object Crypto {
    case class CipherParams(iv: String)

    /**
     * @param n Cost
     * @param r Block size
     * @param p Parallelization
     */
    case class KdfParams(dkLen: Int, n: Int, r: Int, p: Int, salt: String)
  }

  object Encryption {

    sealed abstract class DecryptionFailure
    case object DecodeFailure extends DecryptionFailure
    case object MACMismatch extends DecryptionFailure
    private val kdfN = BigInt(2).pow(18).toInt

    def decrypt(keyFile: KeyFile, password: Password): Either[DecryptionFailure, Bytes] =
      for {
        passwordBytes <- password.getBytes(StandardCharsets.ISO_8859_1).asRight[DecryptionFailure]
        saltBytes     <- Bytes.fromBase58(keyFile.crypto.kdfparams.salt).toRight(DecodeFailure).map(_.toArray)
        derivedKey = deriveKey(passwordBytes, saltBytes)
        cipherBytes <- Bytes.fromBase58(keyFile.crypto.ciphertext).toRight(DecodeFailure).map(_.toArray)
        expectedMAC = Bytes(calculateMAC(derivedKey, cipherBytes))
        macBytes <- Bytes.fromBase58(keyFile.crypto.mac).toRight(DecodeFailure)
        _        <- Either.cond(macBytes === expectedMAC, (), MACMismatch)
        ivBytes  <- Bytes.fromBase58(keyFile.crypto.cipherparams.iv).toRight(DecodeFailure).map(_.toArray)
        outputText = processAES(cipherBytes, derivedKey, ivBytes, forEncryption = false)
      } yield Bytes(outputText)

    def encrypt(plainText: Bytes, address: DionAddress, password: Password): KeyFile = {
      val plainTextBytes = plainText.toArray
      val passwordBytes = password.getBytes(StandardCharsets.ISO_8859_1)
      val random = new java.security.SecureRandom
      val saltBytes = new Array[Byte](32)
      random.nextBytes(saltBytes)
      val ivBytes = new Array[Byte](16)
      random.nextBytes(ivBytes)
      val derivedKey = deriveKey(passwordBytes, saltBytes)

      val cipherText = processAES(plainTextBytes, derivedKey, ivBytes, forEncryption = true)

      val mac = calculateMAC(derivedKey, cipherText)

      KeyFile(
        address = address.allBytes.toBase58,
        KeyFile.Crypto(
          cipher = "aes-128-ctr",
          cipherparams = KeyFile.Crypto.CipherParams(Bytes(ivBytes).toBase58),
          ciphertext = Bytes(cipherText).toBase58,
          kdf = "scrypt",
          kdfparams = KeyFile.Crypto.KdfParams(32, kdfN, 8, 1, Bytes(saltBytes).toBase58),
          mac = Bytes(mac).toBase58
        )
      )
    }

    private def calculateMAC(derivedKey: Array[Byte], cipherText: Array[Byte]): Array[Byte] =
      blake2b256.hash(derivedKey.slice(16, 32) ++ cipherText).value

    private def deriveKey(passwordBytes: Array[Byte], saltBytes: Array[Byte]): Array[Byte] =
      SCrypt.generate(passwordBytes, saltBytes, kdfN, 8, 1, 32)

    private def processAES(
      in:            Array[Byte],
      derivedKey:    Array[Byte],
      ivBytes:       Array[Byte],
      forEncryption: Boolean
    ): Array[Byte] = {
      val cipherParams = new ParametersWithIV(new KeyParameter(derivedKey), ivBytes)
      val aesCtr = new BufferedBlockCipher(new SICBlockCipher(new AESEngine))
      aesCtr.init(forEncryption, cipherParams)

      val outputText = new Array[Byte](in.length)
      aesCtr.processBytes(in, 0, in.length, outputText, 0)
      aesCtr.doFinal(outputText, 0)
      outputText
    }

  }
}
