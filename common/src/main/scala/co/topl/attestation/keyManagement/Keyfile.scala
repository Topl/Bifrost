package co.topl.attestation.keyManagement

import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import co.topl.attestation.Address
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringTypes.implicits._
import co.topl.utils.StringTypes.{Latin1String, Utf8String}
import io.circe.Encoder
import io.circe.syntax.EncoderOps

import java.io.{BufferedWriter, FileWriter}
import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.util.{Failure, Success, Try}

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
  def encryptSecretSafe(secret: S, password: Latin1String)(implicit networkPrefix: NetworkPrefix): KF

  def encryptSecret(secretKey: S, password: String)(implicit
    networkPrefix:             NetworkPrefix
  ): KF = Latin1String.validated(password) match {
    case Valid(str)      => encryptSecretSafe(secretKey, str)
    case Invalid(errors) => throw new Error(s"Password is not Latin-1 encoded: ${errors.show}")
  }

  /**
   * Retrieves the secret key from an encrypted keyfile
   * @param keyfile
   * @param password
   * @param networkPrefix
   * @return
   */
  def decryptSecretSafe(keyfile: KF, password: Latin1String)(implicit networkPrefix: NetworkPrefix): Try[S]

  def decryptSecret(keyfile: KF, password: String)(implicit
    networkPrefix:           NetworkPrefix
  ): Try[S] = Latin1String.validated(password) match {
    case Valid(str)      => decryptSecretSafe(keyfile, str)
    case Invalid(errors) => Failure(new Error(s"Password is not Latin-1 encoded: ${errors.show}"))
  }

  /**
   * Saves an encrypted keyfile to disk
   * @param dir
   * @param password
   * @param secretKey
   * @param networkPrefix
   * @return
   */
  def saveToDiskSafe(dir: Utf8String, password: Latin1String, secretKey: S)(implicit
    networkPrefix:        NetworkPrefix
  ): Try[Unit] = Try {
    // encrypt secret using password
    val kf = encryptSecretSafe(secretKey, password)

    // save the keyfile to disk
    val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
    val w = new BufferedWriter(new FileWriter(s"${dir.value}/$dateString-${kf.address.toString}.json"))
    w.write(kf.asJson.toString)
    w.close()
  }

  def saveToDisk(dir: String, password: String, secretKey: S)(implicit
    networkPrefix:    NetworkPrefix
  ): Try[Unit] =
    (Utf8String.validated(dir), Latin1String.validated(password)).mapN(saveToDiskSafe(_, _, secretKey)) match {
      case Valid(success)  => success
      case Invalid(errors) => Failure(throw new Error(s"Invalid inputs: ${errors.show}"))
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
