package co.topl.consensus

import java.io.File

import co.topl.crypto.{KeyFile, PrivateKey25519}
import co.topl.nodeView.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import co.topl.utils.Logging
import scorex.crypto.encode.Base58
import com.typesafe.scalalogging.StrictLogging

import scala.util.{Failure, Success}

class KeyRing (private var secrets: Set[PrivateKey25519],
               defaultKeyDir: File
           ) extends Logging {

  import KeyRing.getListOfFiles

  type S = PrivateKey25519
  type PI = ProofOfKnowledgeProposition[S]

  def getKeys[A] (f: KeyFile => A): List[A] =
    getListOfFiles(defaultKeyDir)
      .map(file => f(KeyFile.readFile(file.getPath)))

  /**
    * Retrieves the public keys.
    *
    * @return - the public keys as ProofOfKnowledgePropositions
    */
  def publicKeys: Set[PI] = getKeys(_.pubKeyBytes).map(PublicKey25519Proposition(_)).toSet

  /**
    * A list of public keys - found by mapping all of the private keys to their public keys.
    *
    * @return - the list of public keys as strings.
    */
  def listOpenKeyFiles: Set[String] = {
    secrets
      .flatMap(_ match {
        case pkp: PrivateKey25519 => Some(Base58.encode(pkp.publicKeyBytes))
        case _                    => None
      })
  }

  /**
    * Given a public key and password, unlock the associated key file.
    *
    * @param publicKeyString Base58 encoded public key to unlock
    * @param password        - password for the given public key.
    */
  def unlockKeyFile (publicKeyString: String, password: String): Unit = {
    val privKey = checkValid(publicKeyString: String, password: String)

    // ensure no duplicate by comparing privKey strings
    if (!secrets.contains(privKey)) secrets += privKey
    else log.warn(s"$publicKeyString is already unlocked")
  }

  /**
    * Given a public key and password, locks a key file.
    *
    * @param publicKeyString Base58 encoded public key to lock
    * @param password        - password associated with public key.
    */
  def lockKeyFile (publicKeyString: String, password: String): Unit = {
    val privKey = checkValid(publicKeyString: String, password: String)

    // ensure no duplicate by comparing privKey strings
    if (!secrets.contains(privKey)) log.warn(s"$publicKeyString is already locked")
    else secrets -= ( secrets find ( p => p == privKey ) ).get
  }

  /**
    * Check if given publicKey string is valid and contained in the key file directory
    *
    * @param publicKeyString Base58 encoded public key to query
    * @param password password used to decrypt the keyfile
    * @return the relevant PrivateKey25519 to be processed
    */
  private def checkValid (publicKeyString: String, password: String): PrivateKey25519 = {
    val keyfile = PublicKey25519Proposition(publicKeyString) match {
      case Success(queryKey) => getKeys[KeyFile](kf => kf).filter(_.pubKeyBytes sameElements queryKey.pubKeyBytes)
      case Failure(ex)       => throw ex
    }

    assert(keyfile.size == 1, "Cannot find a unique publicKey in key files")

    keyfile.head.getPrivateKey(password) match {
      case Success(privKey) => privKey
      case Failure(e)       => throw e
    }
  }
}

object KeyRing{
  def apply(path: String): KeyRing = {
    val dir = new File(path)
    dir.mkdirs()
    new KeyRing(Set(), dir)
  }

  def getListOfFiles(dir: File): List[File] = {
    if (dir.exists && dir.isDirectory) dir.listFiles.filter(_.isFile).toList
    else List[File]()
  }
}
