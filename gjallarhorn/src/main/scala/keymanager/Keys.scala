package keymanager

import java.io.File

import com.google.common.primitives.Ints
import scorex.util.encode.Base58
import crypto.{PrivateKey25519, PublicKey25519Proposition}
import scorex.crypto.hash.Blake2b256
import utils.Logging

import scala.util.{Failure, Success, Try}

case class Keys(var secrets: Set[PrivateKey25519],
                defaultKeyDir: File) extends Logging {

  import Keys._
  type S = PrivateKey25519
  type PI = PrivateKey25519.PK

  /**
    * Retrieves the public keys.
    * @return - the public keys as ProofOfKnowledgePropositions
    */
  def publicKeys: Set[PI] = {
    secrets.map(_.publicImage)
    /*getListOfFiles(defaultKeyDir).map(file => PublicKey25519Proposition(PublicKey @@ KeyFile.readFile(file.getPath).pubKeyBytes))
      .toSet*/
  }

  /**Find a secret given it's public image */
  private[keymanager] def  secretByPublicImage(publicImage: PI): Option[S] = publicImage match {
    case p: PublicKey25519Proposition => secrets.find(s => s.publicImage == p)
    //case mn: MofNProposition          => secrets.find(s => mn.setOfPubKeyBytes.exists(s.publicImage == PublicKey25519Proposition(_)))
    case _ => None
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
    * @param publicKeyString - public key as string
    * @param password - password for the given public key.
    */
  def unlockKeyFile(publicKeyString: String, password: String): Try[Unit] = Try{
    val privKey: PrivateKey25519 = checkValid(publicKeyString: String, password: String)

    // ensure no duplicate by comparing privKey strings
    if (!secrets.contains(privKey)) {
      //map(p => Base58.encode(p.privKeyBytes)).contains(Base58.encode(privKey.privKeyBytes))) {
      secrets += privKey
    }
    else log.warn(s"$publicKeyString is already unlocked")
  }


  /**
    * Given a public key and password, locks a key file.
    * @param publicKeyString - public key as string
    * @param password - password associated with public key.
    */
  def lockKeyFile(publicKeyString: String, password: String): Try[Unit] = Try{
    val privKey = checkValid(publicKeyString: String, password: String)
    // ensure no duplicate by comparing privKey strings
    if (!secrets.contains(privKey))
      //.map(p => Base58.encode(p.privKeyBytes)).contains(Base58.encode(privKey.privKeyBytes)))
      log.warn(s"$publicKeyString is already locked")
    else secrets -= (secrets find (p => p == privKey)).get
      //(p => Base58.encode(p.privKeyBytes) == Base58.encode(privKey.privKeyBytes))).get
  }

  /**
    *
    * @param password - the password assigned to the new key file
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
    * @param password - password to use for new key
    * @param mnemonic - the phrase
    * @param lang - the language
    * @return
    */
  def importPhrase (password: String, mnemonic: String, lang: String): Try[PublicKey25519Proposition] = Try {
    // create the BIP object used to verify the chosen language
    val bip = Bip39(lang)

    // ensure the phrase is valid
    if (!bip.phraseCheckSum(mnemonic)) throw new Error("Not a valid input phrase!")

    // calculate the new keyfile and return
    val seed = bip.hexToUuid(bip.phraseToHex(mnemonic))
    val (sk, pk) = KeyFile.generateKeyPair(Blake2b256.hash(seed))

    // add secret to the keyring
    secrets += sk

    // return the public image of the key that was added
    pk
  }


  /**
    *
    * @param publicImage - public key proposition for key file to export
    * @param password - password for the key to export
    * @return
    */
  def exportKeyfile (publicImage: PublicKey25519Proposition, password: String): Try[Unit] = Try {
    secretByPublicImage(publicImage) match {
      case Some(sk) =>
        val file = KeyFile(password, sk)
        file.saveToDisk(defaultKeyDir.getAbsolutePath)
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

object Keys{
  def apply (path: String): Keys = {
    val dir = new File(path)
    dir.mkdirs()
    new Keys(Set(), dir)
  }

  def getListOfFiles(dir: File): List[File] = {
    if ( dir.exists && dir.isDirectory ) dir.listFiles.filter(_.isFile).toList
    else List[File]()
  }
}
