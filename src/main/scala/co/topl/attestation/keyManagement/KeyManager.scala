package co.topl.attestation.keyManagement

import akka.actor._
import co.topl.attestation.keyManagement.KeyManager.ForgerView
import co.topl.attestation.{Address, AddressEncoder}
import co.topl.settings.{AppContext, AppSettings, ForgingSettings, PrivateTestnetSettings}
import co.topl.utils.NetworkType._
import co.topl.utils.Logging

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class KeyManager(
  private val initialKeyRing:       KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
  private val initialRewardAddress: Option[Address]
) extends Actor {

  import KeyManager.ReceivableMessages._

  override def receive: Receive = receive(initialKeyRing, initialRewardAddress)

  def receive(keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519], rewardAddress: Option[Address]): Receive = {
    case CreateKey(password)                 => sender() ! keyRing.DiskOps.generateKeyFile(password)
    case UnlockKey(addr, password)           => sender() ! keyRing.DiskOps.unlockKeyFile(addr, password)
    case LockKey(addr)                       => sender() ! keyRing.removeFromKeyring(addr)
    case ImportKey(password, mnemonic, lang) => sender() ! keyRing.importPhrase(password, mnemonic, lang)
    case ListKeys                            => sender() ! keyRing.addresses
    case UpdateRewardsAddress(address)       => sender() ! updateRewardsAddress(keyRing, address)
    case GetRewardsAddress                   => sender() ! rewardAddress.fold("none")(_.toString)
    case GetForgerView                       => sender() ! ForgerView(keyRing.addresses, rewardAddress)
    case SignMessageWithAddress(address: Address, message: Array[Byte]) => sender() ! signMessageWithAddress(address, message, keyRing)
    case GetPublicKeyFromAddress(address: Address) => sender() ! getPublicKeyFromAddress(address, keyRing)
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

  private def signMessageWithAddress(
      address: Address,
      message: Array[Byte],
      keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519]) =
    keyRing.signWithAddress(address)(message)

  private def getPublicKeyFromAddress(address: Address, keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519]) =
    keyRing.lookupPublicKey(address)
}

object KeyManager extends Logging {

  def apply(name: String, settings: AppSettings, appContext: AppContext)(implicit
    system:       ActorSystem,
    ec:           ExecutionContext
  ): ActorRef =
    system.actorOf(props(settings, appContext)(ec, appContext.networkType.netPrefix), name)

  def props(settings: AppSettings, appContext: AppContext)(implicit
    ec:               ExecutionContext,
    np:               NetworkPrefix
  ): Props = {
    val keyRing = getKeyRing(settings)

    val forgingSettings = settings.forging

    generateInitialKeys(keyRing, forgingSettings.privateTestnet)

    val rewardAddress =
      tryGetRewardsAddressFromSettings(forgingSettings, appContext) orElse keyRing.addresses.headOption

    Props(new KeyManager(keyRing, rewardAddress))
  }

  /** Generates the key ring's initial keys if they do not already exist.
    * Only generates if private testnet enabled.
    * @param keyRing The key ring to save keys to.
    * @param privateTestnetSettings The private testnet settings.
    * @return Unit
    */
  private def generateInitialKeys(
    keyRing:                KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    privateTestnetSettings: Option[PrivateTestnetSettings]
  ): Unit =
    privateTestnetSettings foreach { sfp =>
      if (sfp.genesisSeed.nonEmpty && keyRing.addresses.isEmpty) {
        keyRing
          .generateNewKeyPairs(sfp.numTestnetAccts, sfp.genesisSeed)
          .map(keys => keys.map(_.publicImage)) match {
          case Success(_)     => ()
          case Failure(error) => throw error
        }
      }
    }

  private def tryGetRewardsAddressFromSettings(
    forgingSettings: ForgingSettings,
    appContext:      AppContext
  ): Option[Address] =
    forgingSettings.rewardsAddress.flatMap {
      AddressEncoder.fromStringWithCheck(_, appContext.networkType.netPrefix) match {
        case Failure(ex) =>
          log.warn(s"${Console.YELLOW}Unable to set rewards address due to $ex ${Console.RESET}")
          None

        case Success(addr) => Some(addr)
      }
    }

  private def getKeyRing(
    settings:    AppSettings
  )(implicit np: NetworkPrefix): KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] = {
    val keyFileDir = settings.application.keyFileDir
      .ensuring(_.isDefined, "A keyfile directory must be specified")
      .get

    KeyRing[PrivateKeyCurve25519, KeyfileCurve25519](keyFileDir, KeyfileCurve25519)
  }

  val actorName = "keyManager"

  case class ForgerView(addresses: Set[Address], rewardAddr: Option[Address])

  object ReceivableMessages {
    case class UnlockKey(addr: String, password: String)

    case class LockKey(addr: Address)

    case object ListKeys

    case class CreateKey(password: String)

    case class ImportKey(password: String, mnemonic: String, lang: String)

    case object GetRewardsAddress

    case class UpdateRewardsAddress(address: Address)

    case object GetForgerView

    case class SignMessageWithAddress(address: Address, message: Array[Byte])

    case class GetPublicKeyFromAddress(address: Address)
  }

}
