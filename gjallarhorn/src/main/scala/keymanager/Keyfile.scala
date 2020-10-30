package keymanager

import java.io.{BufferedWriter, FileWriter}
import java.lang.reflect.Constructor
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.temporal.ChronoUnit

import crypto.{PrivateKey25519, PrivateKey25519Companion}
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.generators.SCrypt
import org.bouncycastle.crypto.modes.SICBlockCipher
import org.bouncycastle.crypto.params.{KeyParameter, ParametersWithIV}
import org.whispersystems.curve25519.OpportunisticCurve25519Provider
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256, Keccak256}

import scala.util.Try

/**
  * Created by cykoz on 6/22/2017.
  */

case class KeyFile(pubKeyBytes: Array[Byte],
                   cipherText: Array[Byte],
                   mac: Array[Byte],
                   salt: Array[Byte],
                   iv: Array[Byte]) {

  import KeyFile._

  /**
    * Retrieves the private key, given a password.
    * @param password - password for the private key.
    * @return - if the password is valid, returns the private key.
    */
  def getPrivateKey(password: String): Try[PrivateKey25519] = Try {
    val derivedKey = getDerivedKey(password, salt)
    require(Keccak256(derivedKey.slice(16, 32) ++ cipherText) sameElements mac, "MAC does not match. Try again")

    val (decrypted, _) = getAESResult(derivedKey, iv, cipherText, encrypt = false)
    require(pubKeyBytes sameElements getPkFromSk(decrypted), "PublicKey in file is invalid")

    PrivateKey25519(decrypted, pubKeyBytes)
  }

  lazy val json: Json = Map(
    "crypto" -> Map(
      "cipher" -> "aes-128-ctr".asJson,
      "cipherParams" -> Map(
        "iv" -> Base58.encode(iv).asJson
      ).asJson,
      "cipherText" -> Base58.encode(cipherText).asJson,
      "kdf" -> "scrypt".asJson,
      "kdfSalt" -> Base58.encode(salt).asJson,
      "mac" -> Base58.encode(mac).asJson
    ).asJson,
    "publicKeyId" -> Base58.encode(pubKeyBytes).asJson
  ).asJson

  /**
    * Checks if a given key file equals this key file.
    * @param obj - the object being compared to this key file.
    * @return - true if the given object equals this key file, false otherwise.
    */
  override def equals(obj: Any): Boolean = obj match {
    case k: KeyFile => {
      (k.pubKeyBytes sameElements pubKeyBytes) && (k.cipherText sameElements cipherText) &&
        (k.mac sameElements mac) &&
        (k.salt sameElements salt) &&
        (k.iv sameElements iv)
    }
    case _ => false
  }

  override def hashCode(): Int = (BigInt(Blake2b256(pubKeyBytes)) % Int.MaxValue).toInt
}

object KeyFile {

  /**
    * Generates a derived key given a password and a salt.
    * @param password
    * @param salt
    * @return - the derived key as an array of bytes.
    */
  def getDerivedKey(password: String, salt: Array[Byte]): Array[Byte] = {
    SCrypt.generate(password.getBytes(StandardCharsets.UTF_8), salt, scala.math.pow(2, 18).toInt, 8, 1, 32)
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

    val outputText = Array.fill(32)(1: Byte)
    aesCtr.processBytes(inputText, 0, inputText.length, outputText, 0)
    aesCtr.doFinal(outputText, 0)

    (outputText, Keccak256(derivedKey.slice(16, 32) ++ outputText))
  }

  def uuid: String = java.util.UUID.randomUUID.toString

  /**
    * Creates a keyfile.
    * @param password
    * @param seed
    * @param defaultKeyDir
    * @return - returns a temp key file based on the parameters passed in.
    */
  def apply(password: String, seed: Array[Byte] = Blake2b256(uuid), defaultKeyDir: String): KeyFile = {

    val salt = Blake2b256(uuid)

    var (sk, pk) = PrivateKey25519Companion.generateKeys(seed)

    val ivData = Blake2b256(uuid).slice(0, 16)

    val derivedKey = getDerivedKey(password, salt)
    val (cipherText, mac) = getAESResult(derivedKey, ivData, sk.privKeyBytes, encrypt = true)

    val tempFile = KeyFile(pk.pubKeyBytes, cipherText, mac, salt, ivData)

    val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
    val w = new BufferedWriter(new FileWriter(s"$defaultKeyDir/$dateString-${Base58.encode(pk.pubKeyBytes)}.json"))
    w.write(tempFile.json.toString())
    w.close()
    tempFile
  }

  /**
    * Gets public key from private (secret) key.
    * @param sk - secret key (or private key).
    * @return - public key.
    */
  def getPkFromSk(sk: Array[Byte]): Array[Byte] = provider.generatePublicKey(sk)

  /**
    * Reads a file.
    * @param filename - the name of a file.
    * @return - returns the key file with the given file name.
    */
  def readFile(filename: String): KeyFile = {
    val jsonString = scala.io.Source.fromFile(filename).mkString
    val res: Json = parse(jsonString) match {case Right(re) => re; case Left(ex) => throw ex}
    res.as[KeyFile] match {
      case Right(f: KeyFile) => f
      case Left(e) => throw new Exception(s"Could not parse KeyFile: $e")
    }
  }

  implicit val decodeKeyFile: Decoder[KeyFile] = (c: HCursor) => for {
    pubKeyString <- c.downField("publicKeyId").as[String]
    cipherTextString <- c.downField("crypto").downField("cipherText").as[String]
    macString <- c.downField("crypto").downField("mac").as[String]
    saltString <- c.downField("crypto").downField("kdfSalt").as[String]
    ivString <- c.downField("crypto").downField("cipherParams").downField("iv").as[String]
  } yield {
    val pubKey = Base58.decode(pubKeyString).get
    val cipherText = Base58.decode(cipherTextString).get
    val mac = Base58.decode(macString).get
    val salt = Base58.decode(saltString).get
    val iv = Base58.decode(ivString).get
    KeyFile(pubKey, cipherText, mac, salt, iv)
  }

  private val provider: OpportunisticCurve25519Provider = {
    val constructor = classOf[OpportunisticCurve25519Provider]
      .getDeclaredConstructors
      .head
      .asInstanceOf[Constructor[OpportunisticCurve25519Provider]]
    constructor.setAccessible(true)
    constructor.newInstance()
  }
}
