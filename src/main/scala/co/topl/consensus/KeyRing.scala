package co.topl.consensus

import java.io.File

import co.topl.crypto.{ KeyFile, PrivateKey25519 }
import co.topl.nodeView.state.box.proposition.{ MofNProposition, ProofOfKnowledgeProposition, PublicKey25519Proposition }
import co.topl.utils.Logging
import scorex.crypto.encode.Base58

import scala.util.{ Failure, Success }

class KeyRing ( private var secrets: Set[PrivateKey25519],
                defaultKeyDir      : File
              ) extends Logging {

  import KeyRing.getListOfFiles

  type S = PrivateKey25519
  type PI = PrivateKey25519.PK

  /**
   * Retrieves a list of public images for the secrets currently held in the keyring
   *
   * @return - the public keys as ProofOfKnowledgePropositions
   */
  def publicKeys: Set[PI] = secrets.map(_.publicImage).toSet

  /**Find a secret given it's public image */
  private[consnesus] def secretByPublicImage(publicImage: PI): Option[S] = publicImage match {
    case p: PublicKey25519Proposition => secrets.find(s => s.publicImage == p)
    case mn: MofNProposition          => secrets.find(s => mn.setOfPubKeyBytes.exists(s.publicImage == PublicKey25519Proposition(_)))
    case _ => None
  }

  /**
   * Given a public key and password, unlock the associated key file.
   *
   * @param publicKeyString Base58 encoded public key to unlock
   * @param password        - password for the given public key.
   */
  def unlockKeyFile ( publicKeyString: String, password: String ): Unit = {
    val privKey = checkValid(publicKeyString: String, password: String)

    // ensure no duplicate by comparing privKey strings
    if ( !secrets.contains(privKey) ) secrets += privKey
    else log.warn(s"$publicKeyString is already unlocked")
  }

  /**
   * Given a public key and password, locks a key file.
   *
   * @param publicKeyString Base58 encoded public key to lock
   * @param password        - password associated with public key.
   */
  def lockKeyFile ( publicKeyString: String, password: String ): Unit = {
    val privKey = checkValid(publicKeyString: String, password: String)

    // ensure no duplicate by comparing privKey strings
    if ( !secrets.contains(privKey) ) log.warn(s"$publicKeyString is already locked")
    else secrets -= (secrets find (p => p == privKey)).get
  }

  /** Return a list of KeuFile instances for all keys in the key file directory */
  private def listKeyFiles: List[KeyFile] =
    getListOfFiles(defaultKeyDir).map(file => KeyFile.readFile(file.getPath))

  /**
   * Check if given publicKey string is valid and contained in the key file directory
   *
   * @param publicKeyString Base58 encoded public key to query
   * @param password        password used to decrypt the keyfile
   * @return the relevant PrivateKey25519 to be processed
   */
  private def checkValid ( publicKeyString: String, password: String ): PrivateKey25519 = {
    val keyfile = PublicKey25519Proposition(publicKeyString) match {
      case Success(queryKey) => listKeyFiles.filter(_.pubKeyBytes sameElements queryKey.pubKeyBytes)
      case Failure(ex)       => throw ex
    }

    assert(keyfile.size == 1, "Cannot find a unique publicKey in key files")

    keyfile.head.getPrivateKey(password) match {
      case Success(privKey) => privKey
      case Failure(e)       => throw e
    }
  }
}

object KeyRing {
  def apply ( path: String ): KeyRing = {
    val dir = new File(path)
    dir.mkdirs()
    new KeyRing(Set(), dir)
  }

  def getListOfFiles ( dir: File ): List[File] = {
    if ( dir.exists && dir.isDirectory ) dir.listFiles.filter(_.isFile).toList
    else List[File]()
  }
}
