package keymanager

import java.io.File

import scorex.util.encode.Base58
import com.typesafe.scalalogging.StrictLogging
import crypto.{PrivateKey25519, ProofOfKnowledgeProposition, PublicKey25519Proposition}

import scala.util.{Failure, Success}

case class Keys(var secrets: Set[PrivateKey25519], defaultKeyDir: String) extends StrictLogging {

  import Keys._
  type S = PrivateKey25519
  type PI = ProofOfKnowledgeProposition[S]

  /**
    * Retrieves the public keys.
    * @return - the public keys as ProofOfKnowledgePropositions
    */
  def publicKeys: Set[PI] = {
    //secrets.map(_.publicImage)
    getListOfFiles(defaultKeyDir).map(file => PublicKey25519Proposition(KeyFile.readFile(file.getPath).pubKeyBytes))
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
  def unlockKeyFile(publicKeyString: String, password: String): Unit = {
    val keyfiles = getListOfFiles(defaultKeyDir)
      .map(file => KeyFile.readFile(file.getPath))
      .filter(k => k
        .pubKeyBytes sameElements Base58
        .decode(publicKeyString)
        .get)

    assert(keyfiles.size == 1, "Cannot find a unique publicKey in key files")
    val privKey = keyfiles.head.getPrivateKey(password) match {
      case Success(priv) => Set(priv)
      case Failure(e) => throw e
    }
    // ensure no duplicate by comparing privKey strings
    if (!secrets.map(p => Base58.encode(p.privKeyBytes)).contains(Base58.encode(privKey.head.privKeyBytes))) {
      secrets += privKey.head
    } else {
      logger.warn(s"$publicKeyString is already unlocked")
    }
  }

  /**
    * Given a public key and password, locks a key file.
    * @param publicKeyString
    * @param password - password associated with public key.
    */
  def lockKeyFile(publicKeyString: String, password: String): Unit = {
    val keyfiles = getListOfFiles(defaultKeyDir)
      .map(file => KeyFile.readFile(file.getPath))
      .filter(k => k
        .pubKeyBytes sameElements Base58
        .decode(publicKeyString)
        .get)
    assert(keyfiles.size == 1, "Cannot find a unique publicKey in key files")
    val privKey = keyfiles.head.getPrivateKey(password) match {
      case Success(priv) => Set(priv)
      case Failure(e) => throw e
    }
    // ensure no duplicate by comparing privKey strings
    if (!secrets.map(p => Base58.encode(p.privKeyBytes)).contains(Base58.encode(privKey.head.privKeyBytes))) {
      logger.warn(s"$publicKeyString is already locked")
    } else {
      secrets -= (secrets find (p => Base58.encode(p.privKeyBytes) == Base58.encode(privKey.head.privKeyBytes))).get
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
