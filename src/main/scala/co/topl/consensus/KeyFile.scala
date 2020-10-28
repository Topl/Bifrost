package co.topl.consensus

import java.io.{BufferedWriter, FileWriter}
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.temporal.ChronoUnit

import co.topl.crypto.PrivateKey25519
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.generators.SCrypt
import org.bouncycastle.crypto.modes.SICBlockCipher
import org.bouncycastle.crypto.params.{KeyParameter, ParametersWithIV}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.util.Random.randomBytes
import scorex.util.encode.Base58

import scala.util.Try

/**
  * Created by cykoz on 6/22/2017.
  */
case class KeyFile (address    : String,
                    cipherText : Array[Byte],
                    mac        : Array[Byte],
                    salt       : Array[Byte],
                    iv         : Array[Byte]) {

  lazy val publicKeyFromAddress: PublicKey25519Proposition = PublicKey25519Proposition(address)

  private[consensus] def getPrivateKey (password: String): Try[PrivateKey25519] = Try {
    val derivedKey = KeyFile.getDerivedKey(password, salt)
    val calcMAC = KeyFile.getMAC(derivedKey, cipherText)
    require(calcMAC sameElements mac, "MAC does not match. Try again")

    KeyFile.getAESResult(derivedKey, iv, cipherText, encrypt = false) match {
      case (cipherBytes, _) => cipherBytes.grouped(Curve25519.KeyLength).toSeq match {
        case Seq(skBytes, pkBytes) =>
          // recreate the private key
          val privateKey = new PrivateKey25519(PrivateKey @@ skBytes, PublicKey @@ pkBytes)
          // check that the address given in the keyfile matches the public key
          require(publicKeyFromAddress == privateKey.publicImage, "PublicKey in file is invalid")
          privateKey
      }
    }
  }

  private[consensus] def saveToDisk (dir: String): Try[Unit] = Try {
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
    * Recreate a keyfile from the provided seed
    *
    * @param password string used to encrypt the private key when saved to disk
    * @return
    */
  def apply (password: String, secretKey: PrivateKey25519): KeyFile = {
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
    *
    * @param filename
    * @return
    */
  def readFile (filename: String): KeyFile = {
    val jsonString = scala.io.Source.fromFile(filename)
    val key = parse(jsonString.mkString).right.get.as[KeyFile] match {
      case Right(f: KeyFile) => f
      case Left(e)           => throw new Exception(s"Could not parse KeyFile: $e")
    }
    jsonString.close()
    key
  }

  /**
    *
    * @param password
    * @param salt
    * @return
    */
  private def getDerivedKey (password: String, salt: Array[Byte]): Array[Byte] = {
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
    *
    * @param derivedKey
    * @param ivData
    * @param inputText
    * @param encrypt
    * @return
    */
  private def getAESResult (derivedKey: Array[Byte], ivData: Array[Byte], inputText: Array[Byte], encrypt: Boolean):
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
