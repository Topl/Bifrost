package co.topl.credential

import cats.implicits._
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signing.Password
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models._
import com.google.common.primitives.Ints
import io.circe.{Codec, DecodingFailure}
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax._
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.generators.SCrypt
import org.bouncycastle.crypto.modes.SICBlockCipher
import org.bouncycastle.crypto.params.{KeyParameter, ParametersWithIV}

import java.nio.charset.StandardCharsets

/**
 * Describes encrypted data
 */
case class KeyFile(metadata: KeyFile.Metadata, crypto: KeyFile.Crypto)

object KeyFile {

  implicit val cryptoCipherParamsCodec: Codec[Crypto.CipherParams] = deriveCodec
  implicit val cryptokdfParamsCodec: Codec[Crypto.KdfParams] = deriveCodec
  implicit val cryptoCodec: Codec[Crypto] = deriveCodec

  implicit val typedEvidenceCodec: Codec[TypedEvidence] =
    Codec.from(
      a =>
        a
          .as[String]
          .flatMap(Bytes.fromBase58(_).toRight(DecodingFailure("Not base58", Nil)))
          .flatMap(b =>
            (
              Sized.strict[Bytes, Lengths.`32`.type](b.tail).leftMap(_ => DecodingFailure("Invalid length", Nil))
            ).map(TypedEvidence(b.head, _))
          ),
      _.allBytes.toBase58.asJson
    )

  implicit val metadataCodec: Codec[Metadata] = deriveCodec
  implicit val keyFileCodec: Codec[KeyFile] = deriveCodec

  case class Metadata(evidence: TypedEvidence)

  /**
   * @param ciphertext Base58-encoded array.  When decrypted, the array is structured as: [dataLength] ++ [data] ++ [padding]
   * @param mac Base58-encoded
   */
  case class Crypto(
    cipher:       String,
    cipherparams: Crypto.CipherParams,
    ciphertext:   String,
    kdf:          String,
    kdfparams:    Crypto.KdfParams,
    mac:          String
  )

  object Crypto {

    /**
     * @param iv Base58-encoded
     */
    case class CipherParams(iv: String)

    /**
     * @param n Cost
     * @param r Block size
     * @param p Parallelization
     * @param salt Base58-encoded
     */
    case class KdfParams(dkLen: Int, n: Int, r: Int, p: Int, salt: String)
  }

  object Encryption {

    sealed abstract class DecryptionFailure
    case object DecodeFailure extends DecryptionFailure

    /**
     * Indicates password was incorrect
     */
    case object MACMismatch extends DecryptionFailure

    private val kdfN = scala.math.pow(2, 18).toInt

    private val BlockSize = 16

    def decrypt(keyFile: KeyFile, password: Password): Either[DecryptionFailure, Bytes] =
      for {
        passwordBytes   <- password.getBytes(StandardCharsets.ISO_8859_1).asRight[DecryptionFailure]
        saltBytes       <- Bytes.fromBase58(keyFile.crypto.kdfparams.salt).toRight(DecodeFailure).map(_.toArray)
        ciphertextBytes <- Bytes.fromBase58(keyFile.crypto.ciphertext).toRight(DecodeFailure).map(_.toArray)
        macBytes        <- Bytes.fromBase58(keyFile.crypto.mac).toRight(DecodeFailure)
        ivBytes         <- Bytes.fromBase58(keyFile.crypto.cipherparams.iv).toRight(DecodeFailure).map(_.toArray)

        derivedKey = deriveKey(passwordBytes, saltBytes)
        expectedMAC = Bytes(calculateMAC(derivedKey, ciphertextBytes))
        _ <- Either.cond(macBytes === expectedMAC, (), MACMismatch)
        outputText = processAES(ciphertextBytes, derivedKey, ivBytes, forEncryption = false)
        // The outputText consists of [dataLength] ++ [data] ++ [padding]
        dataLength = Ints.fromByteArray(outputText.slice(0, 4))
        data = outputText.slice(4, dataLength + 4)
      } yield Bytes(data)

    def encrypt(plainText: Bytes, metadata: KeyFile.Metadata, password: Password): KeyFile = {
      val plaintextArray = plainText.toArray
      val random = new java.security.SecureRandom()
      val saltBytes = new Array[Byte](32)
      random.nextBytes(saltBytes)
      val ivBytes = new Array[Byte](16)
      random.nextBytes(ivBytes)
      val derivedKey = deriveKey(password.getBytes(StandardCharsets.ISO_8859_1), saltBytes)
      // AES block size is a multiple of 16, so the data must have a length multiple of 16.  Simply padding the
      // bytes would make it impossible to determine the initial data bytes upon encryption.  The length of plaintext
      // is prepended to the plaintext, and _then_ it is padded.
      val paddedBytes =
        Ints.toByteArray(plaintextArray.length) ++
        plaintextArray ++
        Array.fill[Byte]((BlockSize - ((plaintextArray.length + 4) % BlockSize)) % BlockSize)(0)

      val cipherText = processAES(paddedBytes, derivedKey, ivBytes, forEncryption = true)
      val mac = calculateMAC(derivedKey, cipherText)

      KeyFile(
        metadata,
        KeyFile.Crypto(
          cipher = "aes-128-ctr",
          cipherparams = KeyFile.Crypto.CipherParams(Bytes(ivBytes).toBase58),
          ciphertext = Bytes(cipherText).toBase58,
          kdf = "scrypt",
          kdfparams = KeyFile.Crypto.KdfParams(BlockSize, kdfN, 8, 1, Bytes(saltBytes).toBase58),
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

      val outputText = Array.fill[Byte](in.length)(1: Byte)
      aesCtr.processBytes(in, 0, in.length, outputText, 0)
      aesCtr.doFinal(outputText, 0)
      outputText
    }

  }
}
