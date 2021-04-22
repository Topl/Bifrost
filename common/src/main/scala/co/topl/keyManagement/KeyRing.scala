package co.topl.keyManagement

import co.topl.attestation.Address
import co.topl.utils.NetworkType.NetworkPrefix
import com.google.common.primitives.Ints
import co.topl.utils.SecureRandom.randomBytes

import java.io.File
import scala.util.{Failure, Success, Try}

class KeyRing[
  S <: Secret,
  KF <: Keyfile[S]
](defaultKeyDir: File, private var secrets: Set[S], private val keyfileOps: KeyfileCompanion[S, KF])(implicit
  networkPrefix: NetworkPrefix,
  sg:            SecretGenerator[S]
) {

  type PK = S#PK
  type PR = S#PR

  /** Retrieves a list of public images for the secrets currently held in the keyring
    *
    * @return - the public keys as ProofOfKnowledgePropositions
    */
  def addresses: Set[Address] = secrets.map(_.publicImage.address)

  /** Generate a signature using the secret key associated with an Address */
  def signWithAddress(addr: Address)(messageToSign: Array[Byte]): Try[PR] =
    secrets.find(_.publicImage.address == addr) match {
      case Some(sk) => Try(sk.sign(messageToSign))
      case _        => throw new Exception("Unable to find secret for the given address")
    }

  /** Lookup the public key associated with an address */
  def lookupPublicKey(addr: Address): Try[PK] =
    secrets.find(_.publicImage.address == addr) match {
      case Some(sk) => Success(sk.publicImage)
      case _        => throw new Exception("Unable to find secret for the given address")
    }

  /** Generate an attestation map using the given address and message to sign
    * @param addr address to lookup the proposition associated with the proof that is needed
    * @param messageToSign the message that should be committed to
    * @return a map that can be inserted into a transaction
    */
  def generateAttestation(addr: Address)(messageToSign: Array[Byte]): Map[PK, PR] =
    (lookupPublicKey(addr), signWithAddress(addr)(messageToSign)) match {
      case (Success(pk), Success(sig)) => Map(pk -> sig)
      case (_, Failure(e))             => throw e
      case (Failure(e), _)             => throw e // this assumes the failure is due to not finding the address
    }

  /** Generates a new keypair and updates the key ring with the new secret
    * @param num
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

  /** Attempts to import a keyfile into the key ring
    * @param keyfile encrypted keyfile that will be added
    * @param password password for decrypting the keyfile
    * @return the address of the key pair for this network
    */
  def importKeyPair(keyfile: KF, password: String): Try[Address] = {
    require(
      keyfile.address.networkPrefix == networkPrefix,
      s"Invalid key file for chosen network. " +
      s"Provided key has network prefix ${keyfile.address.networkPrefix} but required prefix is $networkPrefix"
    )
    keyfileOps.decryptSecret(keyfile, password).map { s =>
      secrets ++= Set(s)
      s.publicImage.address
    }
  }

  /**
   * Removes an address from the keyring so that it is not available for signing any longer
   * @param address network specific address to be removed
   * @return a try indicating whether the key ring was successfully mutated or not
   */
  def removeFromKeyring(address: Address): Try[Unit] = Try {
    secretByAddress(address) match {
      case Some(sk) => secrets -= (secrets find (p => p == sk)).get
      case None     => throw new Exception("Unable to find address in keyring")
    }
  }

  /** @param password
    * @param mnemonic
    * @param lang
    * @return
    */
  def importPhrase(password: String, mnemonic: String, lang: String)(implicit sg: SecretGenerator[S]): Try[Address] =
    Failure(new Exception("Not yet implemented"))

//  // JAA - 20210301 - Disabling for now so I can decouple the crypto package from Bifrost
//    Try {
//      // create the BIP object used to verify the chosen language
//      val bip = Bip39(lang)
//
//      // ensure the phrase is valid
//      if (!bip.phraseCheckSum(mnemonic)) throw new Error("Not a valid input phrase!")
//
//      // calculate the new keyfile and return
//      val seed = bip.hexToUuid(bip.phraseToHex(mnemonic))
//      val sk = sg.generateSecret(Blake2b256(seed))
//
//      // add secret to the keyring
//      secrets += sk._1
//
//      // return the public image of the key that was added
//      sk._2.address
//    }

  /** Find a secret given it's public image */
  private def secretByAddress(addr: Address): Option[S] =
    secrets.find(_.publicImage.address == addr)

  object DiskOps {

    /** generate a new random key pair and save to disk
      * @param password
      */
    def generateKeyFile(password: String): Try[Address] =
      generateNewKeyPairs().map { sk =>
        exportKeyfileToDisk(sk.head.publicImage.address, password)
        sk.head.publicImage.address
      }

    /** Given am address and password, unlock the associated key file.
      *
      * @param address Base58 encoded address of the key to unlock
      * @param password        - password for the given public key.
      */
    def unlockKeyFile(address: String, password: String): Try[Address] = {
      val keyfile = checkValid(address: String, password: String)
      importKeyPair(keyfile, password)
    }

    /** @param address
      * @param password
      * @return
      */
    private def exportKeyfileToDisk(address: Address, password: String): Try[Unit] = Try {
      secretByAddress(address) match {
        case Some(sk) => keyfileOps.saveToDisk(defaultKeyDir.getAbsolutePath, password, sk)
        case _        => Failure(new Error("Unable to find a matching secret in the key ring"))
      }
    }

    /** Return a list of KeuFile instances for all keys in the key file directory */
    private def listKeyFiles: List[KF] =
      KeyRing.getListOfFiles(defaultKeyDir).map(file => keyfileOps.readFile(file.getPath))

    /** Check if given publicKey string is valid and contained in the key file directory
      *
      * @param address Base58 encoded public key to query
      * @param password        password used to decrypt the keyfile
      * @return the relevant PrivateKey25519 to be processed
      */
    private def checkValid(address: String, password: String): KF = {
      val keyfile = listKeyFiles.filter {
        _.address == Address(networkPrefix)(address)
      }

      require(keyfile.size == 1, s"Cannot find a unique matching keyfile in $defaultKeyDir")
      keyfile.head

//      keyfileOps.decryptSecret(keyfile.head, password) match {
//        case Success(sk) => sk
//        case Failure(e)  => throw e
//      }
    }
  }
}

object KeyRing {

  def apply[
    S <: Secret: SecretGenerator,
    KF <: Keyfile[S]
  ](path: String, keyfileCompanion: KeyfileCompanion[S, KF])(implicit networkPrefix: NetworkPrefix): KeyRing[S, KF] = {
    val dir = new File(path)
    dir.mkdirs()
    new KeyRing(dir, Set(), keyfileCompanion)
  }

  def getListOfFiles(dir: File): List[File] =
    if (dir.exists && dir.isDirectory) dir.listFiles.filter(_.isFile).toList
    else List[File]()
}
