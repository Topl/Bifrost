package keymanager

import java.io.{BufferedWriter, FileWriter}
import java.time.Instant
import java.time.temporal.ChronoUnit

import attestation.{Address, Secret}
import attestation.AddressEncoder.NetworkPrefix
import crypto.KeyfileCurve25519
import io.circe.syntax.EncoderOps
import io.circe.Encoder

import scala.util.Try

/**
 * A keyfile is the form in which keys are encrypted and saved to disk
 * @tparam S the type of secret for the keyfile
 */
trait Keyfile[S <: Secret] {
  val address: Address
  val cipherText: Array[Byte]
}

/**
 * KeyfileCompanion defines methods used to create and save a keyfile or decrypt a keyfile
 * @tparam S the type of Secret
 * @tparam KF the type of keyfile
 */
trait KeyfileCompanion[S <: Secret, KF <: Keyfile[S]] {

  /**
   * Returns an encrypted version of the secret key
   * @param secret - secret associated with the key
   * @param password password for the key to encrypt
   * @param networkPrefix current network prefix
   * @return encrypted version of the secret key
   */
  def encryptSecret(secret: S, password: String)(implicit networkPrefix: NetworkPrefix): KF

  /**
   * Retrieves the secret key from an encrypted keyfile
   * @param keyfile keyfile to retrieve secret for
   * @param password password of given keyfile
   * @param networkPrefix current network prefix
   * @return if given the correct password, returns the secret key, else returns failure
   */
  def decryptSecret(keyfile: KF, password: String)(implicit networkPrefix: NetworkPrefix): Try[S]

  /**
   * Saves an encrypted keyfile to disk
   * @param dir the directory to save the keyfile to
   * @param password the password for the keyfile
   * @param secretKey the secret key for the keyfile to save
   * @param networkPrefix the current network prefix
   * @return Successful if given the current password else fails
   */
  def saveToDisk(dir: String, password: String, secretKey: S)(implicit networkPrefix: NetworkPrefix): Try[Unit] = Try {
    // encrypt secret using password
    val kf = encryptSecret(secretKey, password)

    // save the keyfile to disk
    val dateString = Instant.now().truncatedTo(ChronoUnit.SECONDS).toString.replace(":", "-")
    val w = new BufferedWriter(new FileWriter(s"$dir/$dateString-${kf.address}.json"))
    w.write(kf.asJson.toString)
    w.close()
  }

  /**
   * Reads a given file from disk and attempts to return a keyfile of the correct type
   * @param filename file to be read from disk
   * @return if the filename contains a keyfile, returns keyfile
   */
  def readFile(filename: String)(implicit networkPrefix: NetworkPrefix): KF
}

object Keyfile {

  implicit def jsonEncoder[KF <: Keyfile[_]]: Encoder[KF] = { case kf: KeyfileCurve25519 =>
    KeyfileCurve25519.jsonEncoder(kf)
  }
}
