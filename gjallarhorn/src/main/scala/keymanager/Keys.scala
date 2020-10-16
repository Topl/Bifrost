package keymanager

import java.io.File

import scorex.util.encode.Base58
import crypto.{PrivateKey25519, ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.crypto.signatures.PublicKey
import utils.Logging

import scala.util.{Failure, Success, Try}

case class Keys(var secrets: Set[PrivateKey25519], defaultKeyDir: String) extends Logging {

  import Keys._
  type S = PrivateKey25519
  type PI = ProofOfKnowledgeProposition[S]

  /**
    * Retrieves the public keys.
    * @return - the public keys as ProofOfKnowledgePropositions
    */
  def publicKeys: Set[PI] = {
    //secrets.map(_.publicImage)
    getListOfFiles(defaultKeyDir).map(file => PublicKey25519Proposition(PublicKey @@ KeyFile.readFile(file.getPath).pubKeyBytes))
      .toSet
  }

  /**
    * A list of public keys - found by mapping all of the private keys to their public keys.
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
    * @param publicKeyString
    * @param password - password for the given public key.
    */
  def unlockKeyFile(publicKeyString: String, password: String): Try[Unit] = Try{
    val privKey = checkValid(publicKeyString: String, password: String)

    // ensure no duplicate by comparing privKey strings
    if (!secrets.map(p => Base58.encode(p.privKeyBytes)).contains(Base58.encode(privKey.privKeyBytes))) {
      secrets += privKey
    }
    else log.warn(s"$publicKeyString is already unlocked")
  }


  /**
    * Given a public key and password, locks a key file.
    * @param publicKeyString
    * @param password - password associated with public key.
    */
  def lockKeyFile(publicKeyString: String, password: String): Try[Unit] = Try{
    val privKey = checkValid(publicKeyString: String, password: String)

    // ensure no duplicate by comparing privKey strings
    if (!secrets.map(p => Base58.encode(p.privKeyBytes)).contains(Base58.encode(privKey.privKeyBytes)))
      log.warn(s"$publicKeyString is already locked")
    else secrets -= (secrets find (p => Base58.encode(p.privKeyBytes) == Base58.encode(privKey.privKeyBytes))).get
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
    val keyfile = listKeyFiles.filter {
      _.pubKeyBytes sameElements PublicKey25519Proposition(publicKeyString).pubKeyBytes
    }

    assert(keyfile.size == 1, "Cannot find a unique publicKey in key files")

    keyfile.head.getPrivateKey(password) match {
      case Success(privKey) => privKey
      case Failure(e)       => throw e
    }
  }
}

object Keys{
  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
}
