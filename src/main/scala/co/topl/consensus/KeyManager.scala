package co.topl.consensus

import akka.actor._
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, PrivateKeyCurve25519}
import co.topl.attestation.{Address, AddressEncoder, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.consensus.KeyManager.{AttemptForgingKeyView, ForgerStartupKeyView}
import co.topl.settings.{AppContext, AppSettings}
import co.topl.utils.Logging
import co.topl.utils.NetworkType._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

class KeyManager(implicit network: NetworkPrefix, settings: AppSettings, appContext: AppContext)
  extends Actor with Logging {

  import KeyManager.ReceivableMessages._

  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  override def receive: Receive = {
    val keyRing = createKeyRing()
    val rewardsAddress = tryGetRewardsAddressFromSettings()

    receive(keyRing, rewardsAddress)
  }

  /**
    * Receives messages with the given key ring and reward address set as context data.
    * @param keyRing the current key ring state
    * @param rewardAddress the address to give forging rewards to
    * @return a Receive partial function
    */
  def receive(keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519], rewardAddress: Option[Address]): Receive = {
    case CreateKey(password) => sender() ! keyRing.DiskOps.generateKeyFile(password)
    case UnlockKey(addr, password) => sender() ! keyRing.DiskOps.unlockKeyFile(addr, password)
    case LockKey(addr) => sender() ! keyRing.removeFromKeyring(addr)
    case ImportKey(password, mnemonic, lang) => sender() ! keyRing.importPhrase(password, mnemonic, lang)
    case ListKeys => sender() ! keyRing.addresses
    case UpdateRewardsAddress(address) => sender() ! updateRewardsAddress(keyRing, address)
    case GetRewardsAddress => sender() ! rewardAddress.fold("none")(_.toString)
    case GetForgerStartupKeyView => sender() ! ForgerStartupKeyView(keyRing.addresses, rewardAddress)
    case GetAttemptForgingKeyView => sender() ! getAttemptForgingKeyView(keyRing, rewardAddress)
    case GenerateInititalAddresses => sender() ! generateInitialAddresses(keyRing, rewardAddress)
  }

  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  /** Creates a new key ring. */
  def createKeyRing(): KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] = {
    val keyFileDir = settings.application.keyFileDir
      .ensuring(_.isDefined, "A keyfile directory must be specified")
      .get

    KeyRing[PrivateKeyCurve25519, KeyfileCurve25519](keyFileDir, KeyfileCurve25519)
  }

  /**
    * Generates the initial addresses in the node for a private or local test network.
    * @param keyRing the key ring to generate addresses in
    * @param rewardAddress the current reward address
    * @return a try which results in a ForgerView of the current addresses and rewards address
    */
  private def generateInitialAddresses(
    keyRing:       KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    rewardAddress: Option[Address]
  ): Try[ForgerStartupKeyView] = {
    // If the keyring is not already populated and this is a private/local testnet, generate the keys
    // this is for when you have started up a private network and are attempting to resume it using
    // the same seed you used previously to continue forging
    if (keyRing.addresses.isEmpty && Seq(PrivateTestnet, LocalTestnet).contains(appContext.networkType)) {
      settings.forging.privateTestnet match {
        case Some(sfp) =>
          val (numAccts, seed) = (sfp.numTestnetAccts, sfp.genesisSeed)

          keyRing
            .generateNewKeyPairs(numAccts, seed)
            .map(keys => keys.map(_.publicImage.address))
            .map { addresses =>
              val newRewardAddress = if (rewardAddress.isEmpty) Some(addresses.head) else rewardAddress

              context.become(receive(keyRing, newRewardAddress))

              ForgerStartupKeyView(addresses, newRewardAddress)
            }
        case _ =>
          log.warn("No private testnet settings found!")
          Success(ForgerStartupKeyView(keyRing.addresses, rewardAddress))
      }
    } else {
      Success(ForgerStartupKeyView(keyRing.addresses, rewardAddress))
    }
  }

  private def getAttemptForgingKeyView(
                                        keyRing:KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
                                        rewardAddress: Option[Address]
                                      ) = {
    sender() ! AttemptForgingKeyView(
      keyRing.addresses,
      rewardAddress,
      (address: Address) => (message: Array[Byte]) => keyRing.signWithAddress(address)(message),
      (address: Address) => keyRing.lookupPublicKey(address)
    )
  }

  /**
    * Signs a message using an address in the given key ring.
    * @param address the address to sign with
    * @param message the message to sign
    * @param keyRing contains the address secret to sign with
    * @return a try which results in a proof if successful
    */
  private def signMessageWithAddress(
    address: Address,
    message: Array[Byte],
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519]
  ) =
    keyRing.signWithAddress(address)(message)

  /** Gets a public key from a given address */
  private def getPublicKeyFromAddress(address: Address, keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519]) =
    keyRing.lookupPublicKey(address)

  /** Tries to get a configured rewards address from the forging settings. */
  private def tryGetRewardsAddressFromSettings(): Option[Address] =
    settings.forging.rewardsAddress.flatMap {
      AddressEncoder.fromStringWithCheck(_, appContext.networkType.netPrefix) match {
        case Failure(ex) =>
          log.warn(s"${Console.YELLOW}Unable to set rewards address due to $ex ${Console.RESET}")
          None

        case Success(addr) => Some(addr)
      }
    }

  /** Updates the rewards address from the API */
  private def updateRewardsAddress(
                                    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
                                    address: Address
                                  ): String = {
    val newRewardAddress = Some(address)
    context.become(receive(keyRing, newRewardAddress))
    newRewardAddress.fold("none")(_.toString)
  }
}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object KeyManager extends Logging {

  val actorName = "keyManager"

  case class ForgerStartupKeyView(addresses: Set[Address], rewardAddr: Option[Address])

  case class AttemptForgingKeyView(
    addresses: Set[Address],
    rewardAddr: Option[Address],
    sign: Address => Array[Byte] => Try[SignatureCurve25519],
    getPublicKey: Address => Try[PublicKeyPropositionCurve25519]
  )

  object ReceivableMessages {
    case class UnlockKey(addr: String, password: String)

    case class LockKey(addr: Address)

    case object ListKeys

    case class CreateKey(password: String)

    case class ImportKey(password: String, mnemonic: String, lang: String)

    case object GetRewardsAddress

    case class UpdateRewardsAddress(address: Address)

    case object GetForgerStartupKeyView

    case object GetAttemptForgingKeyView

    case object GenerateInititalAddresses
  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object KeyManagerRef {

  def props(settings: AppSettings, appContext: AppContext)(implicit ec: ExecutionContext, np: NetworkPrefix): Props =
    Props(
      new KeyManager()(np, settings, appContext)
    )

  def apply(name: String, settings: AppSettings, appContext: AppContext)(implicit
    system:       ActorSystem,
    ec:           ExecutionContext
  ): ActorRef =
    system.actorOf(props(settings, appContext)(ec, appContext.networkType.netPrefix), name)

}
