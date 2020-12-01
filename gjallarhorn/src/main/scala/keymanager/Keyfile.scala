package keymanager

import java.io.{BufferedWriter, FileWriter}
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.temporal.ChronoUnit

import crypto.{PrivateKey25519, PublicKey25519Proposition}
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.generators.SCrypt
import org.bouncycastle.crypto.modes.SICBlockCipher
import org.bouncycastle.crypto.params.{KeyParameter, ParametersWithIV}
import scorex.util.encode.Base58
import scorex.crypto.hash.{Blake2b256, Keccak256}
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.util.Random.randomBytes

import scala.util.Try

/**
  * Created by cykoz on 6/22/2017.
  */

case class KeyFile(address    : String,
                   cipherText : Array[Byte],
                   mac        : Array[Byte],
                   salt       : Array[Byte],
                   iv         : Array[Byte]) {


  lazy val publicKeyFromAddress: PublicKey25519Proposition = PublicKey25519Proposition(address)

  /**
    * Retrieves the private key, given a password.
    * @param password - password for the private key.
    * @return - if the password is valid, returns the private key.
    */
  /*private[keymanager]*/ def getPrivateKey(password: String): Try[PrivateKey25519] = Try {
    val derivedKey = KeyFile.getDerivedKey(password, salt)
    val calcMAC = KeyFile.getMAC(derivedKey, cipherText)
    require(calcMAC sameElements mac, "MAC does not match. Try again")

    KeyFile.getAESResult(derivedKey, iv, cipherText, encrypt = false) match {
      case (cipherBytes, _) => cipherBytes.grouped(Curve25519.KeyLength).toSeq match {
        case Seq(skBytes, pkBytes) => {
          // recreate the private key
          val privateKey = new PrivateKey25519(PrivateKey @@ skBytes, PublicKey @@ pkBytes)
          // check that the address given in the keyfile matches the public key
          require(publicKeyFromAddress == privateKey.publicImage, "PublicKey in file is invalid")
          privateKey
        }
      }
    }
  }

  private[keymanager] def saveToDisk (dir: String): Try[Unit] = Try {
    val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
    val w = new BufferedWriter(new FileWriter(s"$dir/$dateString-${this.address}.json"))
    w.write(KeyFile.jsonEncoder(this).toString)
    w.close()
  }
}

object KeyFile {

  implicit val jsonEncoder: Encoder[KeyFile] = { kf: KeyFile ⇒
    Map(
      "crypto" -> Map(
        "cipher" -> "aes-128-ctr".asJson,
        "cipherParams" -> Map("iv" -> Base58.encode(kf.iv).asJson ).asJson,
        "cipherText" -> Base58.encode(kf.cipherText).asJson,
        "kdf" -> "scrypt".asJson,
        "kdfSalt" -> Base58.encode(kf.salt).asJson,
        "mac" -> Base58.encode(kf.mac).asJson
      ).asJson,
      "address" -> kf.address.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[KeyFile] = (c: HCursor) =>
    for {
      address <- c.downField("address").as[String]
      cipherTextString <- c.downField("crypto").downField("cipherText").as[String]
      macString <- c.downField("crypto").downField("mac").as[String]
      saltString <- c.downField("crypto").downField("kdfSalt").as[String]
      ivString <- c.downField("crypto").downField("cipherParams").downField("iv").as[String]
    } yield {
      val cipherText = Base58.decode(cipherTextString).get
      val mac = Base58.decode(macString).get
      val salt = Base58.decode(saltString).get
      val iv = Base58.decode(ivString).get

      new KeyFile(address, cipherText, mac, salt, iv)
    }

  /**
    * Creates a keyfile.
    * @param password
    * @return - returns a temp key file based on the parameters passed in.
    */
  def apply(password: String, secretKey: PrivateKey25519): KeyFile = {
    // get random bytes to obfuscate the cipher
    val salt = randomBytes(32)
    val ivData = randomBytes(16)

    // calculate the deterministic key used to create the cipher
    val derivedKey = getDerivedKey(password, salt)

    // encrypt private key
    val (cipherText, mac) = getAESResult(derivedKey, ivData, secretKey.bytes, encrypt = true)

    new KeyFile(secretKey.publicImage.address, cipherText, mac, salt, ivData)
  }

  /** helper function to create a new random keyfile */
  def generateKeyPair: (PrivateKey25519, PublicKey25519Proposition) = PrivateKey25519.generateKeys(randomBytes(128))

  def generateKeyPair (seed: Array[Byte]): (PrivateKey25519, PublicKey25519Proposition) = PrivateKey25519.generateKeys(seed)

  /**
    * Reads a file.
    * @param filename - the name of a file.
    * @return - returns the key file with the given file name.
    */
  def readFile(filename: String): KeyFile = {
    val jsonString = scala.io.Source.fromFile(filename)
    val key = parse(jsonString.mkString).right.get.as[KeyFile] match {
      case Right(f: KeyFile) => f
      case Left(e) => throw new Exception(s"Could not parse KeyFile: $e")
    }
    jsonString.close()
    key
  }

  /**
    * Generates a derived key given a password and a salt.
    * @param password
    * @param salt
    * @return - the derived key as an array of bytes.
    */
  def getDerivedKey(password: String, salt: Array[Byte]): Array[Byte] = {
    val passwordBytes = password.getBytes(StandardCharsets.UTF_8)
    SCrypt.generate(passwordBytes, salt, scala.math.pow(2, 18).toInt, 8, 1, 32)
  }

  /**
    *
    * @param derivedKey
    * @param cipherText
    * @return
    */
  private def getMAC (derivedKey: Array[Byte], cipherText: Array[Byte]): Array[Byte] = {
    Blake2b256(derivedKey.slice(16, 32) ++ cipherText)
  }

  /**
    * Retrieves the encryption result based on the given parameters.
    * @param derivedKey - generated key.
    * @param ivData - data for the key file.
    * @param inputText - text for the key file.
    * @param encrypt - true for encryption, false for decryption.
    * @return - returns encryption result.
    */
  def getAESResult(derivedKey: Array[Byte], ivData: Array[Byte], inputText: Array[Byte], encrypt: Boolean):
  (Array[Byte], Array[Byte]) = {
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
