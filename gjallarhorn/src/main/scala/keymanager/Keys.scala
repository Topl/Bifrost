package keymanager

import java.io.File

import com.google.common.primitives.Ints
import crypto.AddressEncoder.NetworkPrefix
import crypto.{Address, Secret, SecretGenerator}
import scorex.util.Random.randomBytes
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58
import utils.Logging

import scala.util.{Failure, Success, Try}

class Keys[
  S <: Secret,
  KF <: Keyfile[S]
](defaultKeyDir:          File,
  private var secrets:    Set[S],
  private val keyfileOps: KeyfileCompanion[S, KF]
 )(implicit networkPrefix: NetworkPrefix, sg: SecretGenerator[S]) extends Logging {

  type PR = S#PR

  /** Retrieves a list of public images for the secrets currently held in the keyring
    *
    * @return - the public keys as ProofOfKnowledgePropositions
    */
  def addresses: Set[Address] = secrets.map(_.publicImage.address)

  /** Generate a signature using the secret key associated with an Address */
  def signWithAddress(addr: Address, messageToSign: Array[Byte]): Try[PR] =
    secrets.find(_.publicImage.address == addr) match {
      case Some(sk) => Try(sk.sign(messageToSign))
      case _        => throw new Error("Unable to find secret for the given address")
    }

  /** Lookup the public key associated with an address */
  def lookupPublicKey(addr: Address): Try[S#PK] =
    secrets.find(_.publicImage.address == addr) match {
      case Some(sk) => Success(sk.publicImage)
      case _        => throw new Error("Unable to find secret for the given address")
    }

  /** Given a public key and password, unlock the associated key file.
    *
    * @param publicKeyString Base58 encoded public key to unlock
    * @param password        - password for the given public key.
    */
  def unlockKeyFile(publicKeyString: String, password: String): Try[Unit] = Try {
    val privKey = checkValid(publicKeyString: String, password: String)

    // ensure no duplicate by comparing privKey strings
    if (!secrets.contains(privKey)) secrets += privKey
    else log.warn(s"$publicKeyString is already unlocked")
  }

  /** Given a public key and password, locks a key file.
    *
    * @param publicKeyString Base58 encoded public key to lock
    * @param password        - password associated with public key.
    */
  def lockKeyFile(publicKeyString: String, password: String): Try[Unit] = Try {
    val privKey = checkValid(publicKeyString: String, password: String)

    // ensure no duplicate by comparing privKey strings
    if (!secrets.contains(privKey)) log.warn(s"$publicKeyString is already locked")
    else secrets -= (secrets find (p => p == privKey)).get
  }

  /** @param password
    */
  def generateKeyFile(password: String): Try[Address] = {
    // generate a new random key pair and save to disk
    generateNewKeyPairs().map { sk =>
      exportKeyfile(sk.head.publicImage.address, password)
      sk.head.publicImage.address
    }
  }

  /** @param num
    * @param seedOpt
    * @return
    */
  def generateNewKeyPairs(num: Int = 1, seedOpt: Option[String] = None): Try[Set[S]] =
    Try {
      if (num >= 1) {
        val newSecrets = seedOpt match {
          case Some(seed) => (1 to num).map(i => sg.generateSecret(Ints.toByteArray(i) ++ seed.getBytes())._1).toSet
          case _          => (1 to num).map(_ => sg.generateSecret(randomBytes(128))._1).toSet
        }

        secrets ++= newSecrets
        newSecrets
      } else throw new Error("Number of requested keys must be greater than or equal to 1")
    }

  /** @param password
    * @param mnemonic
    * @param lang
    * @return
    */
  def importPhrase(password: String, mnemonic: String, lang: String)(implicit sg: SecretGenerator[S]): Try[Address] =
    Try {
      // create the BIP object used to verify the chosen language
      val bip = Bip39(lang)

      // ensure the phrase is valid
      if (!bip.phraseCheckSum(mnemonic)) throw new Error("Not a valid input phrase!")

      // calculate the new keyfile and return
      val seed = bip.hexToUuid(bip.phraseToHex(mnemonic))
      val sk = sg.generateSecret(Blake2b256(seed))

      // add secret to the keyring
      secrets += sk._1

      // return the public image of the key that was added
      sk._2.address
    }

  /** @param address
    * @param password
    * @return
    */
  def exportKeyfile(address: Address, password: String): Try[Unit] = Try {
    secretByAddress(address) match {
      case Some(sk) => keyfileOps.saveToDisk(defaultKeyDir.getAbsolutePath, password, sk)
      case _        => Failure(new Error("Unable to find a matching secret in the key ring"))
    }
  }

  /** Find a secret given it's public image */
  private def secretByAddress(addr: Address): Option[S] = {
    secrets.find(_.publicImage.address == addr)
  }

  /** Return a list of KeuFile instances for all keys in the key file directory */
  private def listKeyFiles: List[KF] =
    Keys.getListOfFiles(defaultKeyDir).map(file => keyfileOps.readFile(file.getPath))

  /** Check if given publicKey string is valid and contained in the key file directory
    *
    * @param address Base58 encoded public key to query
    * @param password        password used to decrypt the keyfile
    * @return the relevant PrivateKey25519 to be processed
    */
  private def checkValid(address: String, password: String): S = {
    val keyfile = listKeyFiles.filter {
      _.address == Address(address)
    }

    assert(keyfile.size == 1, s"Cannot find a unique matching keyfile in $defaultKeyDir")

    keyfileOps.decryptSecret(keyfile.head, password) match {
      case Success(sk) => sk
      case Failure(e)  => throw e
    }
  }
}

object Keys {

  def apply[
    S <: Secret: SecretGenerator,
    KF <: Keyfile[S]
  ](path: String, keyfileCompanion: KeyfileCompanion[S, KF])
   (implicit networkPrefix: NetworkPrefix): Keys[S, KF] = {
    val dir = new File(path)
    dir.mkdirs()
    new Keys(dir, Set(), keyfileCompanion)
  }

  def getListOfFiles(dir: File): List[File] = {
    if (dir.exists && dir.isDirectory) dir.listFiles.filter(_.isFile).toList
    else List[File]()
  }
}