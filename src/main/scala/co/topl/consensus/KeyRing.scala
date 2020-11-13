package co.topl.consensus

import java.io.File

import co.topl.attestation.Address
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.proposition.PublicKeyPropositionCurve25519
import co.topl.attestation.secrets.{ PrivateKeyCurve25519, Secret, SecretGenerator }
import co.topl.crypto.{ Bip39, Curve25519KeyFile }
import co.topl.utils.Logging
import scorex.crypto.hash.Blake2b256
import com.google.common.primitives.Ints
import scorex.util.Random.randomBytes

import scala.util.{ Failure, Success, Try }

class KeyRing (private var secrets: Set[PrivateKeyCurve25519], defaultKeyDir: File)
              (implicit networkPrefix: NetworkPrefix, sg: SecretGenerator[PrivateKeyCurve25519])
  extends Logging {

  type PI = PrivateKeyCurve25519#PK

  /**
   * Retrieves a list of public images for the secrets currently held in the keyring
   *
   * @return - the public keys as ProofOfKnowledgePropositions
   */
  def addresses: Set[Address] = secrets.map(_.publicImage.address)

  /**Find a secret given it's public image */
  private[consensus] def secretByAddress (addr: Address): Option[PrivateKeyCurve25519] = {
    secrets.find(_.publicImage.address == addr)
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
  def generateKeyFile (password: String): Try[PI] = {
    // generate a new random key pair and save to disk
    generateNewKeyPairs().map { sk =>
      exportKeyfile(sk.head.publicImage.address, password)
      sk.head.publicImage
    }
  }

  def generateNewKeyPairs (num: Int = 1, seedOpt: Option[String] = None): Try[Set[PrivateKeyCurve25519]] =
    Try {
      if (num >= 1) {
        val newSecrets = seedOpt match {
          case Some(seed) => (1 to num).map(i => sg.generateSecret(Ints.toByteArray(i) ++ seed.getBytes())).toSet
          case _          => (1 to num).map(_ => sg.generateSecret(randomBytes(128))).toSet
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
  def importPhrase (password: String, mnemonic: String, lang: String)(implicit sg: SecretGenerator[PrivateKeyCurve25519]): Try[PI] = Try {
    // create the BIP object used to verify the chosen language
    val bip = Bip39(lang)

    // ensure the phrase is valid
    if (!bip.phraseCheckSum(mnemonic)) throw new Error("Not a valid input phrase!")

    // calculate the new keyfile and return
    val seed = bip.hexToUuid(bip.phraseToHex(mnemonic))
    val sk = sg.generateSecret(Blake2b256(seed))

    // add secret to the keyring
    secrets += sk

    // return the public image of the key that was added
    sk.publicImage
  }

  /**
    *
    * @param address
    * @param password
    * @return
    */
  def exportKeyfile (address: Address, password: String): Try[Unit] = Try {
    secretByAddress(address) match {
      case Some(sk: PrivateKeyCurve25519) => Curve25519KeyFile(password, sk).saveToDisk(defaultKeyDir.getAbsolutePath)
      case _ => Failure(new Error("Unable to find a matching secret in the key ring"))
    }
  }

  /** Return a list of KeuFile instances for all keys in the key file directory */
  private def listKeyFiles: List[Curve25519KeyFile] =
    KeyRing.getListOfFiles(defaultKeyDir).map(file => Curve25519KeyFile.readFile(file.getPath))

  /**
   * Check if given publicKey string is valid and contained in the key file directory
   *
   * @param address Base58 encoded public key to query
   * @param password        password used to decrypt the keyfile
   * @return the relevant PrivateKey25519 to be processed
   */
  private def checkValid ( address: String, password: String ): PrivateKeyCurve25519 = {
    val keyfile = listKeyFiles.filter {
      _.address == Address(address)
    }

    assert(keyfile.size == 1, s"Cannot find a unique matching keyfile in $defaultKeyDir")

    keyfile.head.getPrivateKey(password) match {
      case Success(privKey) => privKey
      case Failure(e)       => throw e
    }
  }
}

object KeyRing {
  def apply[S <: Secret: SecretGenerator] (path: String)(implicit networkPrefix: NetworkPrefix): KeyRing = {
    val dir = new File(path)
    dir.mkdirs()
    new KeyRing(Set(), dir)
  }

  def getListOfFiles (dir: File): List[File] = {
    if ( dir.exists && dir.isDirectory ) dir.listFiles.filter(_.isFile).toList
    else List[File]()
  }
}
