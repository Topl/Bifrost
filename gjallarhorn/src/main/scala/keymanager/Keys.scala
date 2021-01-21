package keymanager

import java.io.File

import attestation.{Address, Secret, SecretGenerator}
import com.google.common.primitives.Ints
import attestation.AddressEncoder.NetworkPrefix
import scorex.util.Random.randomBytes
import scorex.crypto.hash.Blake2b256
import settings.NetworkType
import utils.Logging

import scala.collection.mutable.{Map => MMap}
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
    * @return - list of addresses corresponding to unlocked key files.
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

  private def getPrivateKey(addressString: String, password: String): S = {
    val keyfile = checkValid(addressString: String)

    keyfileOps.decryptSecret(keyfile, password) match {
      case Success(sk) => sk
      case Failure(e) => throw new Exception(s"Wrong password: $e")
    }
  }

  /** Given an address and password, unlock the associated key file.
    *
    * @param addressString Base58 encoded address to unlock
    * @param password        - password for the public key associated with the given address.
    */
  def unlockKeyFile(addressString: String, password: String): Try[Unit] = Try {
    val privKey = getPrivateKey(addressString, password)

    // ensure no duplicate by comparing privKey strings
    if (!secrets.contains(privKey)) secrets += privKey
    else log.warn(s"$addressString is already unlocked")
  }

  /** Given an address and password, locks a key file.
    *
    * @param addressString Base58 encoded address that corresponds to the key to lock
    */
  def lockKeyFile(addressString: String): Try[Unit] = Try {
    val keyfile = checkValid(addressString: String)

    // ensure no duplicate by comparing privKey strings
    val addresses: Set[Address] = secrets.map(sk => sk.publicImage.address)
    if (!addresses.contains(keyfile.address)) log.warn(s"$addressString is already locked")
    else secrets -= (secrets find (p => p.publicImage.address == keyfile.address)).get
  }

  /** @param password - password to use to encrypt generated key.
    */
  def generateKeyFile(password: String, seedOpt: Option[String] = None): Try[Address] = {
    // generate a new random key pair and save to disk
    generateNewKeyPairs(1, seedOpt).map { sk =>
      exportKeyfile(sk.head.publicImage.address, password)
      sk.head.publicImage.address
    }
  }

  /** @param num - number of keys to be generated.
    * @param seedOpt - optional seed to create keys.
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

  /** @param password - password to encrypt imported key.
    * @param mnemonic - mnemonic phrase used to generate key.
    * @param lang - language used to create BIP object to generate key.
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

      exportKeyfile(sk._2.address, password)

      // return the public image of the key that was added
      sk._2.address
    }

  /**
    * Returns the directory for the current network
    * @return
    */
  def getNetworkDir: File = {
    val networkName: String = NetworkType.fromPrefix(networkPrefix) match {
      case Some(network) => network.verboseName
      case None => throw new Error("The network prefix does not match any of the network types!")
    }
    new File(s"${defaultKeyDir.getAbsolutePath}/$networkName")
  }

  /** @param address - address for keyfile to export
    * @param password - password for keyfile to export
    * @return
    */
  def exportKeyfile(address: Address, password: String): Try[Unit] = Try {
    secretByAddress(address) match {
      case Some(sk) =>
        val networkDir = getNetworkDir
        if (!networkDir.exists()) {
          networkDir.mkdirs()
        }
        keyfileOps.saveToDisk(networkDir.getAbsolutePath, password, sk)
      case _        => Failure(new Error("Unable to find a matching secret in the key ring"))
    }
  }

  /** Find a secret given it's public image */
  private def secretByAddress(addr: Address): Option[S] = {
    secrets.find(_.publicImage.address == addr)
  }

  /**
    * Returns a map of address to "locked" or "unlocked"
    * @return
    */
  def listKeyFilesAndStatus: Map[Address, String] = {
    val unlocked: Set[Address] = secrets.map(_.publicImage.address)
    val map: MMap[Address, String] = MMap.empty
    listKeyFiles.map(_.address).foreach(addr =>
      if (unlocked.contains(addr)) map.put(addr, "unlocked")
      else map.put(addr, "locked")
    )
    map.toMap
  }

  /** Return a list of KeyFile instances for all keys in the key file directory for the current network*/
  private def listKeyFiles: List[KF] = {
    Keys.getListOfFiles(getNetworkDir).map(file => keyfileOps.readFile(file.getPath))
  }

  /** Check if given address string is valid and contained in the key file directory
    *
    * @param address Base58 encoded address to query
    * //@param password        password used to decrypt the keyfile
    * @return the relevant PrivateKey25519 to be processed
    */
  private def checkValid(address: String): KF = {
    val keyfile = listKeyFiles.filter {
      _.address == Address(networkPrefix)(address)
    }

    require(keyfile.size == 1, s"Cannot find a unique matching keyfile in $defaultKeyDir")
    keyfile.head
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