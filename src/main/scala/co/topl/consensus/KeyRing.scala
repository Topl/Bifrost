package co.topl.consensus

import java.io.File

import co.topl.crypto.{Bip39, FastCryptographicHash, PrivateKey25519}
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.utils.Logging
import com.google.common.primitives.Ints

import scala.util.{Failure, Success, Try}

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
  private[consensus] def secretByPublicImage(publicImage: PI): Option[S] = publicImage match {
    case p: PublicKey25519Proposition => secrets.find(s => s.publicImage == p)
    //case mn: MofNProposition          => secrets.find(s => mn.setOfPubKeyBytes.exists(s.publicImage == PublicKey25519Proposition(_)))
    case _ => None
  }

  /**
   * Given a public key and password, unlock the associated key file.
   *
   * @param publicKeyString Base58 encoded public key to unlock
   * @param password        - password for the given public key.
   */
  def unlockKeyFile ( publicKeyString: String, password: String ): Try[Unit] = Try{
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
  def lockKeyFile ( publicKeyString: String, password: String ): Try[Unit] = Try{
    val privKey = checkValid(publicKeyString: String, password: String)

    // ensure no duplicate by comparing privKey strings
    if ( !secrets.contains(privKey) ) log.warn(s"$publicKeyString is already locked")
    else secrets -= (secrets find (p => p == privKey)).get
  }

  /**
   *
   * @param password
   */
  def generateKeyFile (password: String): Try[PublicKey25519Proposition] = {
    // generate a new random key pair and save to disk
    generateNewKeyPairs().map { sk =>
      exportKeyfile(sk.head.publicImage, password)
      sk.head.publicImage
    }
  }

  /**  */
  def generateNewKeyPairs (num: Int = 1, seedOpt: Option[String] = None): Try[Set[PrivateKey25519]] = Try {
    if (num >= 1) {
      val newSecrets = seedOpt match {
        case Some(seed) => (1 to num).map(i => KeyFile.generateKeyPair(Ints.toByteArray(i) ++ seed.getBytes())._1).toSet
        case _          => (1 to num).map(_ => KeyFile.generateKeyPair._1).toSet
      }
      secrets ++= newSecrets
      newSecrets
    }
    else throw new Error("Number of requested keys must be greater than or equal to 1")
  }

  /**
   *
   * @param password
   * @param mnemonic
   * @param lang
   * @return
   */
  def importPhrase (password: String, mnemonic: String, lang: String): Try[PublicKey25519Proposition] = Try {
    // create the BIP object used to verify the chosen language
    val bip = Bip39(lang)

    // ensure the phrase is valid
    if (!bip.phraseCheckSum(mnemonic)) throw new Error("Not a valid input phrase!")

    // calculate the new keyfile and return
    val seed = bip.hexToUuid(bip.phraseToHex(mnemonic))
    val (sk, pk) = KeyFile.generateKeyPair(FastCryptographicHash(seed))

    // add secret to the keyring
    secrets += sk

    // return the public image of the key that was added
    pk
  }

  /**
    *
    * @param publicImage
    * @param password
    * @return
    */
  def exportKeyfile (publicImage: PublicKey25519Proposition, password: String): Try[Unit] = Try {
    secretByPublicImage(publicImage) match {
      case Some(sk) => KeyFile(password, sk).saveToDisk(defaultKeyDir.getAbsolutePath)
      case _        => Failure(new Error("Unable to find a matching secret in the key ring"))
    }
  }

  /** Return a list of KeyFile instances for all keys in the key file directory */
  private def listKeyFiles: List[KeyFile] =
    getListOfFiles(defaultKeyDir).map(file => KeyFile.readFile(file.getPath))

  /**
   * Check if given publicKey string is valid and contained in the key file directory
   *
   * @param address Base58 encoded public key to query
   * @param password        password used to decrypt the keyfile
   * @return the relevant PrivateKey25519 to be processed
   */
  private def checkValid ( address: String, password: String ): PrivateKey25519 = {
    val keyfile = listKeyFiles.filter {
      _.publicKeyFromAddress == PublicKey25519Proposition(address)
    }

    assert(keyfile.size == 1, "Cannot find a unique publicKey in key files")

    keyfile.head.getPrivateKey(password) match {
      case Success(privKey) => privKey
      case Failure(e)       => throw e
    }
  }
}

object KeyRing {
  def apply (path: String): KeyRing = {
    val dir = new File(path)
    dir.mkdirs()
    new KeyRing(Set(), dir)
  }

  def getListOfFiles ( dir: File ): List[File] = {
    if ( dir.exists && dir.isDirectory ) dir.listFiles.filter(_.isFile).toList
    else List[File]()
  }
}
