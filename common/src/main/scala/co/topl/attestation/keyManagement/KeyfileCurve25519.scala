package co.topl.attestation.keyManagement

import co.topl.attestation.Address
import co.topl.crypto.hash.blake2b256
import co.topl.crypto.signatures.Curve25519
import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.SecureRandom.randomBytes
import co.topl.utils.StringDataTypes.Latin1Data
import io.circe.parser.parse
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.generators.SCrypt
import org.bouncycastle.crypto.modes.SICBlockCipher
import org.bouncycastle.crypto.params.{KeyParameter, ParametersWithIV}
import co.topl.codecs._

import scala.util.Try

/**
 * Created by cykoz on 6/22/2017.
 */

case class KeyfileCurve25519(
  address:    Address,
  cipherText: Array[Byte],
  mac:        Array[Byte],
  salt:       Array[Byte],
  iv:         Array[Byte]
) extends Keyfile[PrivateKeyCurve25519]

object KeyfileCurve25519Companion extends KeyfileCompanion[PrivateKeyCurve25519, KeyfileCurve25519] {

  /**
   * Create a keyfile from the provided seed and save it to disk
   *
   * @param password string used to encrypt the private key when saved to disk
   * @return
   */
  def encryptSecretSafe(secretKey: PrivateKeyCurve25519, password: Latin1Data)(implicit
    networkPrefix:                 NetworkPrefix
  ): KeyfileCurve25519 = {
    // get random bytes to obfuscate the cipher
    val salt = randomBytes(32)
    val ivData = randomBytes(16)

    // calculate the deterministic key used to create the cipher
    val derivedKey = getDerivedKey(password, salt)

    // encrypt private key
    val (cipherText, mac) = getAESResult(derivedKey, ivData, secretKey.bytes, encrypt = true)

    // generate address from the secret key
    val address = Address.from(secretKey.publicImage)

    // create a new instance of the keyfile case class
    new KeyfileCurve25519(address, cipherText, mac, salt, ivData)
  }

  def decryptSecretSafe(encryptedKeyFile: KeyfileCurve25519, password: Latin1Data)(implicit
    networkPrefix:                        NetworkPrefix
  ): Try[PrivateKeyCurve25519] = Try {
    val derivedKey = getDerivedKey(password, encryptedKeyFile.salt)
    val calcMAC = getMAC(derivedKey, encryptedKeyFile.cipherText)

    require(calcMAC sameElements encryptedKeyFile.mac, "MAC does not match. Try again")

    getAESResult(
      derivedKey,
      encryptedKeyFile.iv,
      encryptedKeyFile.cipherText,
      encrypt = false
    ) match {
      case (cipherBytes, _) =>
        cipherBytes.grouped(Curve25519.KeyLength).toSeq match {
          case Seq(skBytes, pkBytes) =>
            // recreate the private key
            val privateKey = new PrivateKeyCurve25519(PrivateKey(skBytes), PublicKey(pkBytes))
            val derivedAddress = Address.from(privateKey.publicImage)
            // check that the address given in the keyfile matches the public key
            require(
              encryptedKeyFile.address == derivedAddress,
              "Derived address does not match that listed in the keyfile"
            )
            privateKey
        }
    }
  }

  /**
   * @param filename
   * @return
   */
  def readFile(
    filename:               String
  )(implicit networkPrefix: NetworkPrefix): KeyfileCurve25519 = {
    // read data from disk
    val src = scala.io.Source.fromFile(filename)

    // attempt to retrieve the required keyfile type from the data that was just read
    val keyfile = parse(src.mkString) match {
      case Left(ex) => throw ex
      case Right(json) =>
        json.as[KeyfileCurve25519].getOrThrow(ex => new Exception(s"Could not parse KeyFile: $ex"))
    }

    // close the stream and return the keyfile
    src.close()
    keyfile
  }

  /**
   * @param password
   * @param salt
   * @return
   */
  private def getDerivedKey(password: Latin1Data, salt: Array[Byte]): Array[Byte] =
    SCrypt.generate(password.value, salt, scala.math.pow(2, 18).toInt, 8, 1, 32)

  /**
   * @param derivedKey
   * @param cipherText
   * @return
   */
  private def getMAC(derivedKey: Array[Byte], cipherText: Array[Byte]): Array[Byte] =
    blake2b256.hash(derivedKey.slice(16, 32) ++ cipherText).value

  /**
   * @param derivedKey
   * @param ivData
   * @param inputText
   * @param encrypt
   * @return
   */
  private def getAESResult(
    derivedKey: Array[Byte],
    ivData:     Array[Byte],
    inputText:  Array[Byte],
    encrypt:    Boolean
  ): (Array[Byte], Array[Byte]) = {
    val cipherParams = new ParametersWithIV(new KeyParameter(derivedKey), ivData)
    val aesCtr = new BufferedBlockCipher(new SICBlockCipher(new AESEngine))
    aesCtr.init(encrypt, cipherParams)

    val outputText = Array.fill(inputText.length)(1: Byte)
    aesCtr.processBytes(inputText, 0, inputText.length, outputText, 0)
    aesCtr.doFinal(outputText, 0)

    val mac = getMAC(derivedKey, outputText)

    (outputText, mac)
  }
}
