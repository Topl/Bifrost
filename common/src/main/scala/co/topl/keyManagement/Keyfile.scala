package co.topl.keyManagement

import co.topl.attestation.Address
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringTypes.{Latin1String, UTF8String}
import io.circe.Encoder
import io.circe.syntax.EncoderOps

import java.io.{BufferedWriter, FileWriter}
import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.util.Try

trait Keyfile[S <: Secret] {
  val address: Address
  val cipherText: Array[Byte]
}

trait KeyfileCompanion[S <: Secret, KF <: Keyfile[S]] {

  /**
   * Returns an encrypted version of the secret key
   * @param secret
   * @param password
   * @param networkPrefix
   * @return
   */
  def encryptSecret(secret: S, password: Latin1String)(implicit networkPrefix: NetworkPrefix): KF

  /**
   * Retrieves the secret key from an encrypted keyfile
   * @param keyfile
   * @param password
   * @param networkPrefix
   * @return
   */
  def decryptSecret(keyfile: KF, password: Latin1String)(implicit networkPrefix: NetworkPrefix): Try[S]

  /**
   * Saves an encrypted keyfile to disk
   * @param dir
   * @param password
   * @param secretKey
   * @param networkPrefix
   * @return
   */
  def saveToDisk(dir: UTF8String, password: Latin1String, secretKey: S)(implicit
    networkPrefix:    NetworkPrefix
  ): Try[Unit] = Try {
    // encrypt secret using password
    val kf = encryptSecret(secretKey, password)

    // save the keyfile to disk
    val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
    val w = new BufferedWriter(new FileWriter(s"${dir.value}/$dateString-${kf.address.toString}.json"))
    w.write(kf.asJson.toString)
    w.close()
  }

  /**
   * Reads a given file from disk and attempts to return a keyfile of the correct type
   * @param filename file to be read from disk
   * @return
   */
  def readFile(filename: String)(implicit networkPrefix: NetworkPrefix): KF
}

object Keyfile {

  implicit def jsonEncoder[KF <: Keyfile[_]]: Encoder[KF] = { case kf: KeyfileCurve25519 =>
    KeyfileCurve25519.jsonEncoder(kf)
  }
}
