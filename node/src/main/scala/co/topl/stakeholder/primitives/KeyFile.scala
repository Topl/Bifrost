package co.topl.stakeholder.primitives

import java.io.{BufferedWriter, FileWriter}
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.temporal.ChronoUnit
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import io.iohk.iodb.ByteArrayWrapper
import org.bouncycastle.crypto.BufferedBlockCipher
import org.bouncycastle.crypto.engines.AESEngine
import org.bouncycastle.crypto.generators.SCrypt
import org.bouncycastle.crypto.modes.SICBlockCipher
import org.bouncycastle.crypto.params.{KeyParameter, ParametersWithIV}
import co.topl.stakeholder.components.Serializer.DeserializeForgingKey
import co.topl.stakeholder.primitives.Base58
import java.io.File
import com.google.common.primitives.Ints
import co.topl.stakeholder.components.Serializer
import scala.util.{Failure, Success, Try}
import co.topl.stakeholder.primitives.Types.Slot

/**
  * AMS 2020:
  * Key container and file loader, updates keys with secure erasure on disk and encrypts/decrypts local keys
  * @param sig_info encryption info
  * @param vrf_info encryption info
  * @param kes_info encryption info
  * @param fileName this key configuration on disk
  * @param oldFileName previous key configuration on disk as backup
  */

case class KeyFile(sig_info:(Array[Byte],Array[Byte],Array[Byte],Array[Byte],Array[Byte]),
                   vrf_info:(Array[Byte],Array[Byte],Array[Byte],Array[Byte],Array[Byte]),
                   kes_info:(Array[Byte],Array[Byte],Array[Byte],Array[Byte],Array[Byte]),
                   fileName:String,
                   oldFileName:String) {
  import KeyFile._

  def getSigningPrivateKey(password:String,sig:Sig): Try[Array[Byte]] = Try {
    val (
      pubKeyBytes_sig: Array[Byte],
      cipherText: Array[Byte],
      mac: Array[Byte],
      salt: Array[Byte],
      iv: Array[Byte]
      ) = sig_info
    val derivedKey = getDerivedKey(password, salt)
    require(fch.hash(derivedKey.slice(16, 32) ++ cipherText) sameElements mac, "Error: MAC does not match. Try again")
    val (decrypted, _) = getAESResult(derivedKey, iv, cipherText, encrypt = false)
    require(pubKeyBytes_sig sameElements sig.getPkFromSk(decrypted), "Error: PublicKey in file is invalid")
    decrypted
  }

  def getVrfPrivateKey(password: String,vrf:Vrf): Try[Array[Byte]] = Try {
    val (
      pubKeyBytes_vrf: Array[Byte],
      cipherText: Array[Byte],
      mac: Array[Byte],
      salt: Array[Byte],
      iv: Array[Byte]
      ) = vrf_info
    val derivedKey = getDerivedKey(password, salt)
    require(fch.hash(derivedKey.slice(16, 32) ++ cipherText) sameElements mac, "Error: MAC does not match. Try again")
    val (decrypted, _) = getAESResult(derivedKey, iv, cipherText, encrypt = false)
    require(pubKeyBytes_vrf sameElements vrf.getPkFromSk(decrypted), "Error: PublicKey in file is invalid")
    decrypted
  }

  def getKesPrivateKey(password: String,serializer:Serializer,kes:Kes): Try[ForgingKey] = Try {
    val kes_sk_MK = {
      val (
        pubKeyBytes_kes: Array[Byte],
        cipherText: Array[Byte],
        mac: Array[Byte],
        salt: Array[Byte],
        iv: Array[Byte]
        ) = kes_info
      val derivedKey = getDerivedKey(password, salt)
      val (decrypted, mac_check) = decryptAES(derivedKey, iv, cipherText)
      require(mac_check sameElements mac, "Error: MAC does not match")
      val byteStream = new ByteStream(decrypted,None)
      val numBytes = byteStream.getInt
      val decryptedMK = serializer.fromBytes(
        new ByteStream(byteStream.get(numBytes),DeserializeForgingKey)
      ) match {case mk:ForgingKey => mk}
      require(pubKeyBytes_kes sameElements decryptedMK.getPublic(kes), "Error: PublicKey in file is invalid")
      decryptedMK
    }
    kes_sk_MK
  }

  def getKeys(password:String,serializer:Serializer,sig:Sig,vrf:Vrf,kes:Kes):Keys = {
    val out = new Keys
    out.sk_sig = getSigningPrivateKey(password,sig).get
    out.pk_sig = sig_info._1
    out.sk_vrf = getVrfPrivateKey(password,vrf).get
    out.pk_vrf = vrf_info._1
    out.sk_kes = {getKesPrivateKey(password,serializer,kes)} match {
      case Success(value:ForgingKey) => value
      case Failure(exception) =>
        exception.printStackTrace()
        SharedData.throwError
        new ForgingKey
    }
    out.pk_kes = kes_info._1
    out.publicKeys = (out.pk_sig,out.pk_vrf,out.pk_kes)
    out.pkw = ByteArrayWrapper(out.pk_sig++out.pk_vrf++out.pk_kes)
    out
  }

  lazy val json: Json = {
    val map0 = Map("oldFileName" -> oldFileName.asJson,"fileName" -> fileName.asJson)
    val map1 = {
      val (
        pubKeyBytes_sig: Array[Byte],
        cipherText: Array[Byte],
        mac: Array[Byte],
        salt: Array[Byte],
        iv: Array[Byte]
        ) = sig_info
      Map(
        "crypto_sig" -> Map(
          "cipher" -> "aes-256-ctr".asJson,
          "cipherParams" -> Map(
            "iv" -> Base58.encode(iv).asJson
          ).asJson,
          "cipherText" -> Base58.encode(cipherText).asJson,
          "kdf" -> "scrypt".asJson,
          "kdfSalt" -> Base58.encode(salt).asJson,
          "mac" -> Base58.encode(mac).asJson
        ).asJson,
        "publicKey_sig" -> Base58.encode(pubKeyBytes_sig).asJson,
      )
    }
    val map2 = {
      val (
        pubKeyBytes_vrf: Array[Byte],
        cipherText: Array[Byte],
        mac: Array[Byte],
        salt: Array[Byte],
        iv: Array[Byte]
        ) = vrf_info
      Map(
        "crypto_vrf" -> Map(
          "cipher" -> "aes-256-ctr".asJson,
          "cipherParams" -> Map(
            "iv" -> Base58.encode(iv).asJson
          ).asJson,
          "cipherText" -> Base58.encode(cipherText).asJson,
          "kdf" -> "scrypt".asJson,
          "kdfSalt" -> Base58.encode(salt).asJson,
          "mac" -> Base58.encode(mac).asJson
        ).asJson,
        "publicKey_vrf" -> Base58.encode(pubKeyBytes_vrf).asJson,
      )
    }
    val map3 = {
      val (
        pubKeyBytes_kes: Array[Byte],
        cipherText: Array[Byte],
        mac: Array[Byte],
        salt: Array[Byte],
        iv: Array[Byte]
        ) = kes_info
      Map(
        "crypto_kes" -> Map(
          "cipher" -> "aes-256-ctr".asJson,
          "cipherParams" -> Map(
            "iv" -> Base58.encode(iv).asJson
          ).asJson,
          "cipherText" -> Base58.encode(cipherText).asJson,
          "kdf" -> "scrypt".asJson,
          "kdfSalt" -> Base58.encode(salt).asJson,
          "mac" -> Base58.encode(mac).asJson
        ).asJson,
        "publicKey_kes" -> Base58.encode(pubKeyBytes_kes).asJson
      )
    }
    (map0++map1++map2++map3).asJson
  }
}

object KeyFile {
  val fch = new Fch
  def getDerivedKey(password: String, salt: Array[Byte]): Array[Byte] = {
    SCrypt.generate(password.getBytes(StandardCharsets.UTF_8), salt, scala.math.pow(2, 14).toInt, 8, 1, 32)
  }

  def getAESResult(derivedKey: Array[Byte], ivData: Array[Byte], inputText: Array[Byte], encrypt: Boolean):
  (Array[Byte], Array[Byte]) = {
    val cipherParams = new ParametersWithIV(new KeyParameter(derivedKey), ivData)
    val aesCtr = new BufferedBlockCipher(new SICBlockCipher(new AESEngine))
    aesCtr.init(encrypt, cipherParams)

    val outputText = Array.fill(32)(1: Byte)
    aesCtr.processBytes(inputText, 0, inputText.length, outputText, 0)
    aesCtr.doFinal(outputText, 0)

    (outputText, fch.hash(derivedKey.slice(16, 32) ++ outputText))
  }

  def bytes2hex(b: Array[Byte]): String = {
    b.map("%02x" format _).mkString
  }

  def hex2bytes(hex: String): Array[Byte] = {
    if (hex.contains(" ")) {
      hex.split(" ").map(Integer.parseInt(_, 16).toByte)
    } else if (hex.contains("-")) {
      hex.split("-").map(Integer.parseInt(_, 16).toByte)
    } else {
      hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
    }
  }

  def encryptAES(derivedKey: Array[Byte], ivData: Array[Byte], inputText: Array[Byte]):
  (Array[Byte], Array[Byte])= {
    val cipherParams = new ParametersWithIV(new KeyParameter(derivedKey), ivData)
    val aesCtr = new BufferedBlockCipher(new SICBlockCipher(new AESEngine))
    aesCtr.init(true, cipherParams)
    val inBytes = Ints.toByteArray(inputText.length) ++ inputText
    val outputText = Array.fill(16*(inBytes.length/16+1)){0x00.toByte}
    val outLen:Int = aesCtr.processBytes(inBytes, 0, inBytes.length, outputText, 0)
    val outLen2:Int = aesCtr.doFinal(outputText, outLen)
    (outputText, fch.hash(derivedKey.slice(16, 32) ++ outputText))
  }

  def decryptAES(derivedKey: Array[Byte], ivData: Array[Byte], inputText: Array[Byte]):
  (Array[Byte], Array[Byte])= {
    val cipherParams = new ParametersWithIV(new KeyParameter(derivedKey), ivData)
    val aesCtr = new BufferedBlockCipher(new SICBlockCipher(new AESEngine))
    aesCtr.init(false, cipherParams)
    val outputText = Array.fill(inputText.length){0x00.toByte}
    val outLen:Int = aesCtr.processBytes(inputText, 0, inputText.length, outputText, 0)
    val outLen2:Int = aesCtr.doFinal(outputText, outLen)
    (outputText, fch.hash(derivedKey.slice(16, 32) ++ inputText))
  }

  def uuid: String = java.util.UUID.randomUUID.toString

  def empty:KeyFile = new KeyFile(
    (Array(),Array(),Array(),Array(),Array()),
    (Array(),Array(),Array(),Array(),Array()),
    (Array(),Array(),Array(),Array(),Array()),
    "",
    ""
  )

  def fromSeed(
             password:String,
             defaultKeyDir: String,
             serializer: Serializer,
             sig:Sig,
             vrf:Vrf,
             kes:Kes,
             slot:Slot,
             seed1:Array[Byte]=fch.hash(uuid),
             seed2:Array[Byte]=fch.hash(uuid),
             seed3:Array[Byte]=fch.hash(uuid)
  ):KeyFile = {
    val newKeys = Keys.seedKeysSecure(seed1,seed2,seed3,sig,vrf,kes,slot)
    val sig_info = {
      val salt = fch.hash(uuid)
      val ivData = fch.hash(uuid).slice(0, 16)
      val derivedKey = getDerivedKey(password, salt)
      val (cipherText, mac) = getAESResult(derivedKey, ivData, newKeys.get.sk_sig, encrypt = true)
      (
        newKeys.get.pk_sig,
        cipherText,
        mac,
        salt,
        ivData
      )
    }
    val vrf_info = {
      val salt = fch.hash(uuid)
      val ivData = fch.hash(uuid).slice(0, 16)
      val derivedKey = getDerivedKey(password, salt)
      val (cipherText, mac) = getAESResult(derivedKey, ivData, newKeys.get.sk_vrf, encrypt = true)
      (
        newKeys.get.pk_vrf,
        cipherText,
        mac,
        salt,
        ivData
      )
    }
    val kes_info = {
      val salt = fch.hash(uuid)
      val ivData = fch.hash(uuid).slice(0, 16)
      val derivedKey = getDerivedKey(password, salt)
      val keyBytes:Array[Byte] = serializer.getBytes(newKeys.get.sk_kes)
      val (cipherText, mac) = encryptAES(derivedKey, ivData, keyBytes)
      (
        newKeys.get.pk_kes,
        cipherText,
        mac,
        salt,
        ivData
      )
    }
    val dateString = Instant.now().truncatedTo(ChronoUnit.MILLIS).toString.replace(":", "-")
    val fileName = s"$defaultKeyDir/$dateString-${Base58.encode(newKeys.get.pkw.data)}.json"
    val tempFile = new KeyFile(sig_info,vrf_info,kes_info,fileName,"NEWKEY")
    val file = new File(fileName)
    file.getParentFile.mkdirs
    val w = new BufferedWriter(new FileWriter(file))
    w.write(tempFile.json.toString())
    w.close()
    tempFile
  }

  def update(
              keyFile:KeyFile,
              forgingKey: ForgingKey,
              password:String,
              defaultKeyDir: String,
              serializer: Serializer,
              salt:Array[Byte] = fch.hash(uuid),
              derivedKey:Array[Byte] = Array()
            ):KeyFile = {
    val sig_info = keyFile.sig_info
    val vrf_info = keyFile.vrf_info
    val kes_info = {
      val ivData = fch.hash(uuid).slice(0, 16)
      val (cipherText, mac) = if (derivedKey.isEmpty) {
        encryptAES(getDerivedKey(password, salt), ivData, serializer.getBytes(forgingKey))
      } else {
        encryptAES(derivedKey, ivData, serializer.getBytes(forgingKey))
      }
      (
        keyFile.kes_info._1: Array[Byte],
        cipherText: Array[Byte],
        mac: Array[Byte],
        salt: Array[Byte],
        ivData: Array[Byte]
      )
    }
    val dateString = Instant.now().truncatedTo(ChronoUnit.MILLIS).toString.replace(":", "-")
    val fileName = s"$defaultKeyDir/$dateString-${Base58.encode(keyFile.sig_info._1++keyFile.vrf_info._1++keyFile.kes_info._1)}.json"
    val tempFile = KeyFile(sig_info,vrf_info,kes_info,fileName,keyFile.fileName)
    val w = new BufferedWriter(new FileWriter(fileName))
    w.write(tempFile.json.toString())
    w.close()
    deleteFile(keyFile.oldFileName)
    tempFile
  }

  def readFile(filename:String): KeyFile = {
    val jsonString:String = {
      val src = scala.io.Source.fromFile(filename)
      val out = src.mkString
      src.close()
      out
    }
    parse(jsonString).right.get.as[KeyFile] match {
      case Right(f: KeyFile) => f
      case Left(e) => throw new Exception(s"Could not parse KeyFile: $e")
    }
  }

  def restore(storageDir:String): Option[KeyFile] = {
    def getListOfFiles(dir: String):List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
        d.listFiles.filter(_.isFile).toList
      } else {
        List[File]()
      }
    }
    var recoveredKey:Option[KeyFile] = None
    var files = getListOfFiles(s"$storageDir/")

    while (files.nonEmpty) {
      Try{readFile(files.head.getPath)} match {
        case Success(keyFile:KeyFile) =>
          recoveredKey match {
            case None => recoveredKey = Some(keyFile)
            case _ => deleteFile(files.head.getPath)
          }
          files = files.tail
        case Failure(_) =>
          deleteFile(files.head.getPath)
          files = files.tail
      }
    }
    recoveredKey
  }

  def deleteFile(filename:String):Unit = {
    filename match {
      case "NEWKEY" =>
      case _ => new File(filename).delete()
    }
  }

  implicit val decodeKeyFile: Decoder[KeyFile] = (c: HCursor) => for {
    fileName <- c.downField("fileName").as[String]
    oldFileName <- c.downField("oldFileName").as[String]
    pubKeyString_sig <-     c.downField("publicKey_sig").as[String]
    cipherTextString_sig <-    c.downField("crypto_sig").downField("cipherText").as[String]
    macString_sig <-           c.downField("crypto_sig").downField("mac").as[String]
    saltString_sig <-          c.downField("crypto_sig").downField("kdfSalt").as[String]
    ivString_sig <-            c.downField("crypto_sig").downField("cipherParams").downField("iv").as[String]
    pubKeyString_vrf <-     c.downField("publicKey_vrf").as[String]
    cipherTextString_vrf <-    c.downField("crypto_vrf").downField("cipherText").as[String]
    macString_vrf <-           c.downField("crypto_vrf").downField("mac").as[String]
    saltString_vrf <-          c.downField("crypto_vrf").downField("kdfSalt").as[String]
    ivString_vrf <-            c.downField("crypto_vrf").downField("cipherParams").downField("iv").as[String]
    pubKeyString_kes <-     c.downField("publicKey_kes").as[String]
    cipherTextString_kes <-    c.downField("crypto_kes").downField("cipherText").as[String]
    macString_kes <-           c.downField("crypto_kes").downField("mac").as[String]
    saltString_kes <-          c.downField("crypto_kes").downField("kdfSalt").as[String]
    ivString_kes <-            c.downField("crypto_kes").downField("cipherParams").downField("iv").as[String]
  } yield {
    val sig_info = {
      val pubKey =         Base58.decode(pubKeyString_sig).get
      val cipherText = Base58.decode(cipherTextString_sig).get
      val mac =               Base58.decode(macString_sig).get
      val salt =             Base58.decode(saltString_sig).get
      val iv =                 Base58.decode(ivString_sig).get
      (
        pubKey: Array[Byte],
        cipherText: Array[Byte],
        mac: Array[Byte],
        salt: Array[Byte],
        iv: Array[Byte]
      )
    }
    val vrf_info = {
      val pubKey =         Base58.decode(pubKeyString_vrf).get
      val cipherText = Base58.decode(cipherTextString_vrf).get
      val mac =               Base58.decode(macString_vrf).get
      val salt =             Base58.decode(saltString_vrf).get
      val iv =                 Base58.decode(ivString_vrf).get
      (
        pubKey: Array[Byte],
        cipherText: Array[Byte],
        mac: Array[Byte],
        salt: Array[Byte],
        iv: Array[Byte]
      )
    }
    val kes_info = {
      val pubKey =         Base58.decode(pubKeyString_kes).get
      val cipherText = Base58.decode(cipherTextString_kes).get
      val mac =               Base58.decode(macString_kes).get
      val salt =             Base58.decode(saltString_kes).get
      val iv =                 Base58.decode(ivString_kes).get
      (
        pubKey: Array[Byte],
        cipherText: Array[Byte],
        mac: Array[Byte],
        salt: Array[Byte],
        iv: Array[Byte]
      )
    }
    new KeyFile(sig_info,vrf_info,kes_info,fileName,oldFileName)
  }
}