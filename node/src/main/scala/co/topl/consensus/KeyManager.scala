package co.topl.consensus

import akka.Done
import akka.actor._
import akka.util.Timeout
import cats.data.EitherT
import cats.implicits._
import co.topl.attestation.implicits._
import co.topl.attestation.keyManagement.{
  KeyRing,
  KeyfileCurve25519,
  KeyfileCurve25519Companion,
  PrivateKeyCurve25519,
  Secret
}
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.catsakka.AskException
import co.topl.settings.AppSettings
import co.topl.utils.NetworkType._
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import co.topl.utils.catsinstances.implicits._
import co.topl.utils.{Logging, SecureRandom}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

/** Actor that manages the keyRing and reward address */
class KeyManager(settings: AppSettings)(implicit networkPrefix: NetworkPrefix) extends Actor with Logging {

  import KeyManager.ReceivableMessages._
  import KeyManager._

  // //////////////////////////////////////////////////////////////////////////////////
  // //////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

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
    case CreateKey(password) => sender() ! keyRing.DiskOps.generateKeyFile(Latin1Data.unsafe(password))
    case UnlockKey(addr, password) =>
      sender() ! keyRing.DiskOps.unlockKeyFile(Base58Data.unsafe(addr), Latin1Data.unsafe(password))
    case LockKey(addr)                       => sender() ! keyRing.removeFromKeyring(addr)
    case ImportKey(password, mnemonic, lang) => sender() ! keyRing.importPhrase(password, mnemonic, lang)
    case ListKeys                            => sender() ! keyRing.addresses
    case ExportOpenKeyfiles(passwords, path) =>
      sender() ! keyRing.DiskOps.exportOpenKeyfiles(passwords.map(Latin1Data.unsafe), path)
    case GetOpenKeys                   => sender() ! keyRing.getOpenKeys
    case UpdateRewardsAddress(address) => sender() ! updateRewardsAddress(keyRing, address)
    case GetRewardsAddress             => sender() ! rewardAddress.fold("none")(_.show)
    case GetKeyView                    => sender() ! getKeyView(keyRing, rewardAddress)
    case GenerateInitialAddresses(addressSettings) =>
      sender() ! generateInitialAddresses(keyRing, rewardAddress)(addressSettings)
  }

  // //////////////////////////////////////////////////////////////////////////////////
  // ////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  /** Creates a new key ring. */
  def createKeyRing(): KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] = {
    implicit val keyfileCurve25519Companion: KeyfileCurve25519Companion.type = KeyfileCurve25519Companion

    val keyFileDir = settings.application.keyFileDir
      .ensuring(_.isDefined, "A keyfile directory must be specified")
      .get

    KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519](Some(keyFileDir))
  }

  /**
   * Generates the initial addresses in the node for a private or local test network.
   * NOTE: The keyManager will apply the default settings to a Private testnet to ease launching a development node
   * @param keyRing the key ring to generate addresses in
   * @param rewardAddress the current reward address
   * @return a try which results in a ForgerView of the current addresses and rewards address
   */
  private def generateInitialAddresses(
    keyRing:                   KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    rewardAddress:             Option[Address]
  )(addressGenerationSettings: Option[AddressGenerationSettings])(implicit
    networkPrefix:             NetworkPrefix
  ): Try[StartupKeyView] = {
    def addKeys(settings: AddressGenerationSettings): Try[StartupKeyView] =
      keyRing
        .generateNewKeyPairs(settings.numberOfAddresses, settings.addressSeedOpt)
        .map { keys =>
          keys.map(_.publicImage.address)
        }
        .map { addresses =>
          val newRewardAddress = if (rewardAddress.isEmpty) Some(addresses.head) else rewardAddress
          context.become(receive(keyRing, newRewardAddress))
          StartupKeyView(addresses, newRewardAddress)
        }

    addressGenerationSettings match {
      case Some(settings)                                    => addKeys(settings)
      case None if networkPrefix == PrivateTestnet.netPrefix => addKeys(KeyManager.AddressGenerationSettings.default)
      case None => Success(StartupKeyView(keyRing.addresses, rewardAddress))
    }
  }

  /** Gets a read-only view of the key ring to use for forging. */
  private def getKeyView(
    keyRing:       KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    rewardAddress: Option[Address]
  ): KeyView =
    KeyView(
      keyRing.addresses,
      rewardAddress,
      keyRing.signWithAddress,
      keyRing.lookupPublicKey
    )

  /** Tries to get a configured rewards address from the forging settings. */
  private def tryGetRewardsAddressFromSettings(): Option[Address] =
    settings.forging.rewardsAddress.flatMap {
      Base58Data.unsafe(_).decodeAddress.toEither match {
        case Left(ex) =>
          log.warn(s"${Console.YELLOW}Unable to set rewards address due to $ex ${Console.RESET}")
          None

        case Right(addr) => Some(addr)
      }
    }

  /** Updates the rewards address from the API */
  private def updateRewardsAddress(
    keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519],
    address: Address
  ): String = {
    val newRewardAddress = Some(address)
    context.become(receive(keyRing, newRewardAddress))
    newRewardAddress.fold("none")(_.show)
  }
}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object KeyManager {

  val actorName = "keyManager"

  case class AddressGenerationSettings(numberOfAddresses: Int, addressSeedOpt: Option[String])

  object AddressGenerationSettings {
    val default: AddressGenerationSettings = AddressGenerationSettings(10, Some(SecureRandom.randomBytes().mkString))
  }

  case class StartupKeyView(addresses: Set[Address], rewardAddr: Option[Address])

  case class KeyView(
    addresses:    Set[Address],
    rewardAddr:   Option[Address],
    sign:         Address => Array[Byte] => Try[SignatureCurve25519],
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

    case object GetKeyView

    case class GenerateInitialAddresses(addressGenerationSettings: Option[AddressGenerationSettings])

    case class ExportOpenKeyfiles(passwords: List[String], path: String)

    case object GetOpenKeys
  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object KeyManagerRef {

  def props(settings: AppSettings)(implicit networkPrefix: NetworkPrefix): Props =
    Props(
      new KeyManager(settings)
    ).withDispatcher("bifrost.application.key-manager.dispatcher")

}

sealed trait UnlockKeyFailure
case class UnlockKeyFailureException(throwable: Throwable) extends UnlockKeyFailure
sealed trait LockKeyFailure
case class LockKeyFailureException(throwable: Throwable) extends LockKeyFailure
sealed trait CreateKeyFailure
case class CreateKeyFailureException(throwable: Throwable) extends CreateKeyFailure
sealed trait ImportKeyFailure
case class ImportKeyFailureException(throwable: Throwable) extends ImportKeyFailure
sealed trait GetRewardsAddressFailure
case class GetRewardsAddressFailureException(throwable: Throwable) extends GetRewardsAddressFailure
sealed trait UpdateRewardsAddressFailure
case class UpdateRewardsAddressFailureException(throwable: Throwable) extends UpdateRewardsAddressFailure
sealed trait ListOpenKeyfilesFailure
case class ListOpenKeyfilesFailureException(throwable: Throwable) extends ListOpenKeyfilesFailure
sealed trait ExportOpenKeyfilesFailure
case class ExportOpenKeyfilesFailureException(throwable: Throwable) extends ExportOpenKeyfilesFailure
sealed trait GetOpenKeysFailure
case class GetOpenKeysFailureException(throwable: Throwable) extends GetOpenKeysFailure

trait KeyManagerInterface {
  def unlockKey(address:  String, password: String): EitherT[Future, UnlockKeyFailure, Address]
  def lockKey(address:    Address): EitherT[Future, LockKeyFailure, Done.type]
  def createKey(password: String): EitherT[Future, CreateKeyFailure, Address]
  def importKey(password: String, mnemonic: String, lang: String): EitherT[Future, ImportKeyFailure, Address]
  def getRewardsAddress: EitherT[Future, GetRewardsAddressFailure, String]
  def updateRewardsAddress(address: Address): EitherT[Future, UpdateRewardsAddressFailure, Done.type]
  def listOpenKeyfiles(): EitherT[Future, ListOpenKeyfilesFailure, Set[Address]]
  def getOpenKeys: EitherT[Future, GetOpenKeysFailure, Set[_ <: Secret]]

  def exportOpenKeyfiles(
    passwords: List[String],
    path:      String
  ): EitherT[Future, ExportOpenKeyfilesFailure, List[Address]]

}

class ActorKeyManagerInterface(actorRef: ActorRef)(implicit ec: ExecutionContext, timeout: Timeout)
    extends KeyManagerInterface {
  import co.topl.catsakka.CatsActor._

  override def unlockKey(address: String, password: String): EitherT[Future, UnlockKeyFailure, Address] =
    actorRef
      .askEither[Try[Address]](KeyManager.ReceivableMessages.UnlockKey(address, password))
      .leftMap { case AskException(throwable) => UnlockKeyFailureException(throwable) }
      .subflatMap(_.toEither.leftMap(UnlockKeyFailureException))

  override def lockKey(address: Address): EitherT[Future, LockKeyFailure, Done.type] =
    actorRef
      .askEither[Try[_]](KeyManager.ReceivableMessages.LockKey(address))
      .leftMap { case AskException(throwable) => LockKeyFailureException(throwable) }
      .subflatMap(_.toEither.leftMap(LockKeyFailureException))
      .map(_ => Done)
      .leftMap(e => e: LockKeyFailure)

  override def createKey(password: String): EitherT[Future, CreateKeyFailure, Address] =
    actorRef
      .askEither[Try[Address]](KeyManager.ReceivableMessages.CreateKey(password))
      .leftMap { case AskException(throwable) => CreateKeyFailureException(throwable) }
      .subflatMap(_.toEither.leftMap(CreateKeyFailureException))
      .leftMap(e => e: CreateKeyFailure)

  override def importKey(
    password: String,
    mnemonic: String,
    lang:     String
  ): EitherT[Future, ImportKeyFailure, Address] =
    actorRef
      .askEither[Try[Address]](KeyManager.ReceivableMessages.ImportKey(password, mnemonic = mnemonic, lang = lang))
      .leftMap { case AskException(throwable) => ImportKeyFailureException(throwable) }
      .subflatMap(_.toEither.leftMap(ImportKeyFailureException))
      .leftMap(e => e: ImportKeyFailure)

  override def getRewardsAddress: EitherT[Future, GetRewardsAddressFailure, String] =
    actorRef
      .askEither[String](KeyManager.ReceivableMessages.GetRewardsAddress)
      .leftMap { case AskException(throwable) => GetRewardsAddressFailureException(throwable) }
      .leftMap(e => e: GetRewardsAddressFailure)

  override def updateRewardsAddress(address: Address): EitherT[Future, UpdateRewardsAddressFailure, Done.type] =
    actorRef
      .askEither[String](KeyManager.ReceivableMessages.UpdateRewardsAddress(address))
      .leftMap { case AskException(throwable) => UpdateRewardsAddressFailureException(throwable) }
      .leftMap(e => e: UpdateRewardsAddressFailure)
      .map(_ => Done)

  override def listOpenKeyfiles(): EitherT[Future, ListOpenKeyfilesFailure, Set[Address]] =
    actorRef
      .askEither[Set[Address]](KeyManager.ReceivableMessages.ListKeys)
      .leftMap { case AskException(throwable) => ListOpenKeyfilesFailureException(throwable) }
      .leftMap(e => e: ListOpenKeyfilesFailure)

  override def exportOpenKeyfiles(
    passwords: List[String],
    path:      String
  ): EitherT[Future, ExportOpenKeyfilesFailure, List[Address]] =
    actorRef
      .askEither[Try[List[Address]]](KeyManager.ReceivableMessages.ExportOpenKeyfiles(passwords, path))
      .leftMap { case AskException(throwable) => ExportOpenKeyfilesFailureException(throwable) }
      .subflatMap(_.toEither.leftMap(ExportOpenKeyfilesFailureException))
      .leftMap(e => e: ExportOpenKeyfilesFailure)

  override def getOpenKeys: EitherT[Future, GetOpenKeysFailure, Set[_ <: Secret]] =
    actorRef
      .askEither[Set[_ <: Secret]](KeyManager.ReceivableMessages.GetOpenKeys)
      .leftMap {
        case AskException(throwable) => GetOpenKeysFailureException(throwable)
      }
      .leftMap(e => e: GetOpenKeysFailure)
}
