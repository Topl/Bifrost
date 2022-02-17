package co.topl.attestation.keyManagement

import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import co.topl.attestation.Address
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.catsinstances.implicits._
import io.circe.syntax._

import java.io.{BufferedWriter, FileWriter}
import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.util.{Failure, Try}

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
  def encryptSecretSafe(secret: S, password: Latin1Data)(implicit networkPrefix: NetworkPrefix): KF

  def encryptSecret(secretKey: S, password: String)(implicit
    networkPrefix:             NetworkPrefix
  ): KF = Latin1Data.validated(password) match {
    case Valid(str)      => encryptSecretSafe(secretKey, str)
    case Invalid(errors) => throw new Error(s"Password is not Latin-1 encoded: $errors")
  }

  /**
   * Retrieves the secret key from an encrypted keyfile
   * @param keyfile
   * @param password
   * @param networkPrefix
   * @return
   */
  def decryptSecretSafe(keyfile: KF, password: Latin1Data)(implicit networkPrefix: NetworkPrefix): Try[S]

  def decryptSecret(keyfile: KF, password: String)(implicit
    networkPrefix:           NetworkPrefix
  ): Try[S] = Latin1Data.validated(password) match {
    case Valid(str)      => decryptSecretSafe(keyfile, str)
    case Invalid(errors) => Failure(new Error(s"Password is not Latin-1 encoded: $errors"))
  }

  /**
   * Saves an encrypted keyfile to disk
   * @param dir
   * @param password
   * @param secretKey
   * @param networkPrefix
   * @return
   */
  def saveToDiskSafe(dir: String, password: Latin1Data, secretKey: S)(implicit
    networkPrefix:        NetworkPrefix
  ): Try[Unit] = Try {
    // encrypt secret using password
    val kf = encryptSecretSafe(secretKey, password)

    // save the keyfile to disk
    val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
    val w = new BufferedWriter(new FileWriter(s"$dir/$dateString-${kf.address.show}.json"))
    w.write(kf.asJson.toString)
    w.close()
  }

  def saveToDisk(dir: String, password: String, secretKey: S)(implicit
    networkPrefix:    NetworkPrefix
  ): Try[Unit] =
    Latin1Data.validated(password).map(saveToDiskSafe(dir, _, secretKey)) match {
      case Valid(success)  => success
      case Invalid(errors) => Failure(throw new Error(s"Invalid inputs: $errors"))
    }

  /**
   * Reads a given file from disk and attempts to return a keyfile of the correct type
   * @param filename file to be read from disk
   * @return
   */
  def readFile(filename: String)(implicit networkPrefix: NetworkPrefix): KF
}

object Keyfile {}
