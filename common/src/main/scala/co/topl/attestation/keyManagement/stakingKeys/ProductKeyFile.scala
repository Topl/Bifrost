package co.topl.attestation.keyManagement.stakingKeys

import com.google.common.primitives.Ints
import co.topl.crypto.kes.keys._
import co.topl.crypto.hash.blake2b256
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.generators.SCrypt
import org.bouncycastle.crypto.modes.SICBlockCipher
import org.bouncycastle.crypto.params.{KeyParameter, ParametersWithIV}
import co.topl.utils.encode.Base58

import java.io.{BufferedWriter, File, FileWriter, RandomAccessFile}
import java.nio.charset.StandardCharsets
import java.security.SecureRandom
import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.util.{Failure, Success, Try}

/**
 * File loader for evolving keys, updates KES key with secure erasure on disk and encrypts/decrypts the keys
 * @param kes_info encryption info for KES keypair
 * @param fileName this key configuration on disk
 * @param oldFileName previous key configuration on disk as backup
 */

case class ProductKeyFile(kes_info: CipherInfo, fileName: String, oldFileName: String) {
  import ProductKeyFile._

  /**
   * Decrypt the symmetricKey  from kes_info cipher information
   * @param password pass phrase used in AES encryption scheme
   * @return
   */
  def getKey(password: String): Try[SymmetricKey] = decryptKesSK(password)

  private def decryptKesSK(password: String): Try[SymmetricKey] = Try {
    val derivedKey = getDerivedKey(password, kes_info.salt)
    val (decrypted, mac_check) = decryptAES(derivedKey, kes_info.iv, kes_info.cipherText)
    require(mac_check sameElements kes_info.mac, "Error: MAC does not match")
    val decryptedKey = SymmetricKey(ProductPrivateKey.deserializeProductKey(decrypted))
    require(kes_info.pubKey sameElements decryptedKey.getVerificationKey.bytes, "Error: PublicKey in file is invalid")
    decryptedKey
  }

  /**
   * Json representation written to disk
   */

  lazy val json: Json = {
    val map0 = Map("oldFileName" -> oldFileName.asJson, "fileName" -> fileName.asJson)
    val map1 =
      Map(
        "crypto_kes" -> Map(
          "cipher" -> "aes-256-ctr".asJson,
          "cipherParams" -> Map(
            "iv" -> Base58.encode(kes_info.iv).asJson
          ).asJson,
          "cipherText" -> Base58.encode(kes_info.cipherText).asJson,
          "kdf"        -> "scrypt".asJson,
          "kdfSalt"    -> Base58.encode(kes_info.salt).asJson,
          "mac"        -> Base58.encode(kes_info.mac).asJson
        ).asJson,
        "publicKey_kes" -> Base58.encode(kes_info.pubKey).asJson
      )
    (map0 ++ map1).asJson
  }
}

object ProductKeyFile {

  /**
   * Create a new symmetricKey and save it to the specified directory
   * @param password pass phrase used in AES encryption scheme
   * @param defaultKeyDir file folder location for storage
   * @param offset offset of the key
   * @return new key file with randomly generated public private key pairs
   */
  def newRandomKeyFile(
    password:      String,
    defaultKeyDir: String,
    offset:        Long
  ): ProductKeyFile = {
    val newKey = SymmetricKey.newFromSeed(blake2b256.hash(uuid).value, offset)
    val kes_info = {
      val salt = blake2b256.hash(uuid).value
      val ivData = blake2b256.hash(uuid).value.slice(0, 16)
      val derivedKey = getDerivedKey(password, salt)
      val keyBytes: Array[Byte] = newKey.getBytes
      val (cipherText, mac) = encryptAES(derivedKey, ivData, keyBytes)
      CipherInfo(newKey.getVerificationKey.bytes, cipherText, mac, salt, ivData)
    }
    val dateString = Instant.now().truncatedTo(ChronoUnit.MILLIS).toString.replace(":", "-")
    val fileName = s"$defaultKeyDir/$dateString-${Base58.encode(newKey.getVerificationKey.bytes)}.json"
    val newKeyFile = new ProductKeyFile(kes_info, fileName, "NEWKEY")
    val file = new File(fileName)
    file.getParentFile.mkdirs
    val w = new BufferedWriter(new FileWriter(file))
    w.write(newKeyFile.json.toString())
    w.close()
    newKeyFile
  }

  def newKeyFile(
    password:      String,
    defaultKeyDir: String,
    symmetricKey:  SymmetricKey
  ): ProductKeyFile = {
    val kes_info = {
      val salt = blake2b256.hash(uuid).value
      val ivData = blake2b256.hash(uuid).value.slice(0, 16)
      val derivedKey = getDerivedKey(password, salt)
      val keyBytes: Array[Byte] = symmetricKey.getBytes
      val (cipherText, mac) = encryptAES(derivedKey, ivData, keyBytes)
      CipherInfo(symmetricKey.getVerificationKey.bytes, cipherText, mac, salt, ivData)
    }
    val dateString = Instant.now().truncatedTo(ChronoUnit.MILLIS).toString.replace(":", "-")
    val fileName = s"$defaultKeyDir/$dateString-${Base58.encode(symmetricKey.getVerificationKey.bytes)}.json"
    val newKeyFile = new ProductKeyFile(kes_info, fileName, "NEWKEY")
    val file = new File(fileName)
    file.getParentFile.mkdirs
    val w = new BufferedWriter(new FileWriter(file))
    w.write(newKeyFile.json.toString())
    w.close()
    newKeyFile
  }

  /**
   * Update procedure with secure erasure of old files
   * @param keyFile old key file to be updated
   * @param updatedKey key at a time step higher than that presently in keyFile
   * @param password pass phrase used in AES encryption scheme
   * @param defaultKeyDir file folder location for storage
   * @param salt random bytes used to as an input to derivedKey
   * @param derivedKey hardened encryption key input to AES scheme
   * @return
   */

  def updateKeyFile(
    keyFile:       ProductKeyFile,
    updatedKey:    SymmetricKey,
    password:      String,
    defaultKeyDir: String,
    salt:          Array[Byte] = blake2b256.hash(uuid).value,
    derivedKey:    Array[Byte] = Array()
  ): Option[ProductKeyFile] = Try {
    val kes_info = {
      val ivData = blake2b256.hash(uuid).value.slice(0, 16)
      val (cipherText, mac) = if (derivedKey.isEmpty) {
        encryptAES(getDerivedKey(password, salt), ivData, updatedKey.getBytes)
      } else {
        encryptAES(derivedKey, ivData, updatedKey.getBytes)
      }
      CipherInfo(keyFile.kes_info.pubKey, cipherText, mac, salt, ivData)
    }
    val dateString = Instant.now().truncatedTo(ChronoUnit.MILLIS).toString.replace(":", "-")
    val fileName = s"$defaultKeyDir/" +
      s"$dateString-${Base58.encode(keyFile.kes_info.pubKey)}.json"
    val newKeyFile = ProductKeyFile(kes_info, fileName, keyFile.fileName)
    val w = new BufferedWriter(new FileWriter(fileName))
    w.write(newKeyFile.json.toString())
    w.close()
    deleteOldFile(keyFile.oldFileName)
    newKeyFile
  }.toOption

  /**
   * Parses specified key file into key file
   * @param filename path of the json file
   * @return
   */

  def readFile(filename: String): ProductKeyFile = {
    val jsonString: String = {
      val src = scala.io.Source.fromFile(filename)
      val out = src.mkString
      src.close()
      out
    }
    parse(jsonString) match {
      case Right(f: Json) =>
        f.as[ProductKeyFile]
          .getOrElse(
            throw new Exception("Product Key File could not be parsed from Json")
          )
      case Left(e) => throw e
    }
  }

  /**
   * Restores the most recent key file in the specified directory
   * @param storageDir folder containing one or more json key files
   * @return
   */

  def restore(storageDir: String): Option[ProductKeyFile] = {
    def getListOfFiles(dir: String): List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }
    var recoveredKey: Option[ProductKeyFile] = None
    var files = getListOfFiles(s"$storageDir/")
    while (files.nonEmpty)
      Try(readFile(files.head.getPath)) match {
        case Success(keyFile: ProductKeyFile) =>
          recoveredKey match {
            case None => recoveredKey = Some(keyFile)
            case _    => deleteOldFile(files.head.getPath)
          }
          files = files.tail
        case Failure(_) =>
          deleteOldFile(files.head.getPath)
          files = files.tail
      }
    recoveredKey
  }

  private def getDerivedKey(password: String, salt: Array[Byte]): Array[Byte] =
    SCrypt.generate(password.getBytes(StandardCharsets.UTF_8), salt, scala.math.pow(2, 14).toInt, 8, 1, 32)

  private def getAESResult(
    derivedKey: Array[Byte],
    ivData:     Array[Byte],
    inputText:  Array[Byte],
    encrypt:    Boolean
  ): (Array[Byte], Array[Byte]) = {
    val cipherParams = new ParametersWithIV(new KeyParameter(derivedKey), ivData)
    val aesCtr = new BufferedBlockCipher(new SICBlockCipher(new AESEngine))
    aesCtr.init(encrypt, cipherParams)
    val outputText = Array.fill(32)(1: Byte)
    aesCtr.processBytes(inputText, 0, inputText.length, outputText, 0)
    aesCtr.doFinal(outputText, 0)
    (outputText, blake2b256.hash(derivedKey.slice(16, 32) ++ outputText).value)
  }

  private def encryptAES(
    derivedKey: Array[Byte],
    ivData:     Array[Byte],
    inputText:  Array[Byte]
  ): (Array[Byte], Array[Byte]) = {
    val cipherParams = new ParametersWithIV(new KeyParameter(derivedKey), ivData)
    val aesCtr = new BufferedBlockCipher(new SICBlockCipher(new AESEngine))
    aesCtr.init(true, cipherParams)
    val inBytes = Ints.toByteArray(inputText.length) ++ inputText
    val outputText = Array.fill(16 * (inBytes.length / 16 + 1))(0x00.toByte)
    val outLen: Int = aesCtr.processBytes(inBytes, 0, inBytes.length, outputText, 0)
    val outLen2: Int = aesCtr.doFinal(outputText, outLen)
    (outputText, blake2b256.hash(derivedKey.slice(16, 32) ++ outputText).value)
  }

  private def decryptAES(
    derivedKey: Array[Byte],
    ivData:     Array[Byte],
    inputText:  Array[Byte]
  ): (Array[Byte], Array[Byte]) = {
    val cipherParams = new ParametersWithIV(new KeyParameter(derivedKey), ivData)
    val aesCtr = new BufferedBlockCipher(new SICBlockCipher(new AESEngine))
    aesCtr.init(false, cipherParams)
    val outputText = Array.fill(inputText.length)(0x00.toByte)
    val outLen: Int = aesCtr.processBytes(inputText, 0, inputText.length, outputText, 0)
    val outLen2: Int = aesCtr.doFinal(outputText, outLen)
    (outputText, blake2b256.hash(derivedKey.slice(16, 32) ++ inputText).value)
  }

  private def uuid: Array[Byte] = java.util.UUID.randomUUID.toString.getBytes

  private def deleteOldFile(filename: String): Unit =
    filename match {
      case "NEWKEY" =>
      case _        => secureDelete(new File(filename))
    }

  def secureDelete(file: File): Unit =
    if (file.exists) {
      val length = file.length
      val random = new SecureRandom()
      val raf = new RandomAccessFile(file, "rws")
      raf.seek(0)
      raf.getFilePointer
      val data = new Array[Byte](64)
      var pos = 0
      while (pos < length) {
        random.nextBytes(data)
        raf.write(data)
        pos += data.length
      }
      raf.close()
      file.delete()
    }

  implicit private val decodeKeyFile: Decoder[ProductKeyFile] = (c: HCursor) =>
    for {
      fileName             <- c.downField("fileName").as[String]
      oldFileName          <- c.downField("oldFileName").as[String]
      pubKeyString_kes     <- c.downField("publicKey_kes").as[String]
      cipherTextString_kes <- c.downField("crypto_kes").downField("cipherText").as[String]
      macString_kes        <- c.downField("crypto_kes").downField("mac").as[String]
      saltString_kes       <- c.downField("crypto_kes").downField("kdfSalt").as[String]
      ivString_kes         <- c.downField("crypto_kes").downField("cipherParams").downField("iv").as[String]
    } yield {
      val kes_info = CipherInfo(
        Base58.decode(pubKeyString_kes).toOption.get,
        Base58.decode(cipherTextString_kes).toOption.get,
        Base58.decode(macString_kes).toOption.get,
        Base58.decode(saltString_kes).toOption.get,
        Base58.decode(ivString_kes).toOption.get
      )
      new ProductKeyFile(kes_info, fileName, oldFileName)
    }
}
