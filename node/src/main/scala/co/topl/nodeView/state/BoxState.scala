package co.topl.nodeView.state

import cats.data.ValidatedNec
import cats.implicits._
import co.topl.attestation.Address
import co.topl.attestation.implicits._
import co.topl.codecs._
import co.topl.codecs.binary.legacy.modifier.ModifierIdSerializer
import co.topl.codecs.binary.legacy.modifier.box.BoxSerializer
import co.topl.codecs.binary.typeclasses.Persistable
import co.topl.db.LDBVersionedStore
import co.topl.modifier.{ModifierId, ProgramId}
import co.topl.modifier.block.Block
import co.topl.modifier.box._
import co.topl.modifier.transaction._
import co.topl.modifier.transaction.validation._
import co.topl.modifier.transaction.validation.implicits._
import co.topl.nodeView.{KeyValueStore, LDBKeyValueStore}
import co.topl.settings.AppSettings
import co.topl.utils.Logging
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.encode.Base58
import co.topl.utils.implicits._

import java.io.File
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * BifrostState is a data structure which deterministically defines whether an arbitrary transaction is valid and so
 * applicable to it or not. Also has methods to get a closed box, to apply a persistent modifier, and to roll back
 * to a previous version.
 *
 * @param storage singleton storage instance
 * @param version blockId used to identify each block. Also used for rollback
 *                //@param timestamp timestamp of the block that results in this state
 */
case class BoxState(
  override val version:                  ModifierId,
  protected val storage:                 KeyValueStore,
  private[state] val tokenBoxRegistry:   TokenBoxRegistry,
  private[state] val programBoxRegistry: ProgramBoxRegistry
)(implicit networkPrefix:                NetworkPrefix)
    extends MinimalBoxState[Block, BoxState]
    with StoreInterface
    with Logging
    with AutoCloseable {

  override type NVCT = BoxState

  override def close(): Unit = {
    log.info("Attempting to close state storage")
    super.close()

    log.info("Attempting to close token box registry storage")
    tokenBoxRegistry.close()

    log.info("Attempting to close program box registry storage")
    programBoxRegistry.close()
  }

  /**
   * Accessor method to retrieve box data from the state storage
   *
   * @param id unique identifier where the box data is stored
   * @return
   */
  override def getBox(id: BoxId): Option[Box[_]] =
    getFromStorage(id.hash.value)
      .map(BoxSerializer.parseBytes)
      .flatMap(_.toOption)

  /**
   * Accessor method to retrieve program box data from state by abstracting the
   * registry lookup and subsequent state access
   *
   * @param key the program id of the program box to retrieve
   * @tparam PBX the type of box that you are expecting to get back (StateBox, ExecBox, etc.)
   * @return a program box of the specified type if found at the given program is
   */
  override def getProgramBox[PBX <: ProgramBox: ClassTag](key: ProgramId): Option[PBX] =
    programBoxRegistry.getBox(key, getReader) match {
      case Some(box: PBX) => Some(box)
      case _              => None
    }

  /**
   * Accessor method to retrieve a set of token boxes from state that are owned by a
   * certain public key. This works by abstracting the registry lookup and subsequent state access
   *
   * @param key the public key to find boxes for
   * @return a sequence of token boxes held by the public key
   */
  override def getTokenBoxes(key: Address): Option[Seq[TokenBox[TokenValueHolder]]] =
    tokenBoxRegistry.getBox(key, getReader)

  /**
   * Lookup a sequence of boxIds from the appropriate registry.
   * These boxIds can then be used in `getBox` to retrieve the box data.
   *
   * @param key storage key used to identify value(s) in registry
   * @return a sequence of boxes stored beneath the specified key
   */
  def registryLookup[K](key: K): Option[Seq[BoxId]] =
    key match {
      case k: TokenBoxRegistry.K   => tokenBoxRegistry.lookup(k)
      case k: ProgramBoxRegistry.K => programBoxRegistry.lookup(k)
      case _                       => None
    }

  /**
   * Revert version of state to a specific version
   *
   * @param version tag marking a specific state within the store
   * @return an instance of State at the version given
   */
  override def rollbackTo(version: ModifierId): Try[NVCT] = Try {

    // throwing error here since we should stop attempting updates if any part fails
    val updatedTBR =
      tokenBoxRegistry.rollbackTo(version) match {
        case Success(updates) => updates
        case Failure(ex)      => throw new Error(s"Failed to rollback TBR with error\n$ex")
      }

    val updatedPBR =
      programBoxRegistry.rollbackTo(version) match {
        case Success(updates) => updates
        case Failure(ex)      => throw new Error(s"Failed to rollback PBR with error\n$ex")
      }

    if (storage.latestVersionId().exists(_ sameElements version.persistedBytes)) {
      this
    } else {
      log.debug(s"Rollback State to $version from version ${this.version.show}")
      storage.rollbackTo(version.persistedBytes)

      BoxState(version, storage, updatedTBR, updatedPBR)
    }
  }

  /**
   * Public method used to update an instance of state with a new set of transactions
   *
   * @param block block to be applied to state
   * @return a new instance of state with the transaction within mod applied
   */
  override def applyModifier(block: Block): Try[BoxState] = {
    // extract the state changes to be made
    // using option for TBR and PBR since we can skip if the registries aren't present
    val stateChanges = StateChanges(block)
    val tokenChanges = TokenRegistryChanges(block).toOption
    val programChanges = ProgramRegistryChanges(block).toOption

    // if the registries update successfully, attempt to update the utxo storage
    stateChanges match {
      case Success(sc) => applyChanges(block.id, sc, tokenChanges, programChanges)
      case Failure(_)  => throw new Error(s"Failed to calculate state changes for block: ${block.id}")
    }
  }

  /**
   * Apply a series of changes to the internal stores of the state objects
   *
   * @param newVersion version id of this update
   * @param stateChanges changes to be made to state storage
   * @param tokenChanges changes to be made to the token box registry
   * @param programChanges changes to be made to the program box registry
   * @return an updated version of state
   */
  private[state] def applyChanges(
    newVersion:     ModifierId,
    stateChanges:   StateChanges,
    tokenChanges:   Option[TokenRegistryChanges] = None,
    programChanges: Option[ProgramRegistryChanges] = None
  ): Try[NVCT] =
    Try {

      // Filtering boxes pertaining to public keys specified in settings file
      val boxesToAdd = stateChanges.toAppend.map(b => b.id.hash.value -> Persistable[Box[_]].persistedBytes(b))

      val boxIdsToRemove = stateChanges.boxIdsToRemove.map(b => b.hash.value)

      // enforce that the input id's must not match any of the output id's (added emptiness checks for testing)
      require(
        !boxesToAdd.map(_._1).forall(boxIdsToRemove.contains) || (boxesToAdd.isEmpty || boxIdsToRemove.isEmpty),
        s"Attempted application of invalid state"
      )

      log.debug(
        s"Attempting update to State from version ${version.show} to version $newVersion. " +
        s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b))}. " +
        s"Adding boxes ${boxesToAdd.map(b => Base58.encode(b._1))}."
      )

      storage
        .latestVersionId()
        .foreach(latestVersion =>
          stateChanges.boxIdsToRemove
            .foreach { id =>
              require(
                getBox(id).isDefined,
                s"Box id: $id not found in state version: " +
                s"${Base58.encode(latestVersion)}. Aborting state update"
              )
            }
        )

      // throwing error here since we should stop attempting updates if any part fails
      val updatedTBR = tokenChanges
        .map { tc =>
          tokenBoxRegistry.update(newVersion, tc.toRemove, tc.toAppend) match {
            case Success(updates) => updates
            case Failure(ex)      => throw new Error(s"Failed to update TBR with error\n$ex")
          }
        }
        .getOrElse(tokenBoxRegistry)

      val updatedPBR = programChanges
        .map { pc =>
          programBoxRegistry.update(newVersion, pc.toRemove, pc.toUpdate) match {
            case Success(updates) => updates
            case Failure(ex)      => throw new Error(s"Failed to update PBR with error\n$ex")
          }
        }
        .getOrElse(programBoxRegistry)

      storage.update(newVersion.persistedBytes, boxIdsToRemove, boxesToAdd)

      // create updated instance of state
      val newState = BoxState(newVersion, storage, updatedTBR, updatedPBR)

      // enforce that a new valid state must have emptied all boxes to remove
      // boxIdsToRemove.foreach(box => require(newState.getBox(box).isEmpty, s"Box $box is still in state"))

      newState
    } match {
      case Success(newState) =>
        log.debug(s"${Console.GREEN} Update State to version: $newVersion ${Console.RESET}")
        Success(newState)

      case Failure(ex) =>
        log.debug(s"${Console.RED} Failed to update State to version: $newVersion. ${Console.RESET} $ex")
        Failure(ex)
    }

  /**
   * @param transaction
   * @return
   */
  def semanticValidate(transaction: Transaction.TX)(implicit
    networkPrefix:                  NetworkPrefix
  ): ValidatedNec[SemanticValidationFailure, Transaction.TX] =
    transaction.semanticValidation(getReader)
}

object BoxState extends Logging {

  /**
   * @param settings
   * @param initialBlocks
   * @param networkPrefix
   * @return
   */
  def genesisState(settings: AppSettings, initialBlocks: Seq[Block])(implicit networkPrefix: NetworkPrefix): BoxState =
    initialBlocks
      .foldLeft(readOrGenerate(settings)) { (state, mod) =>
        state.applyModifier(mod).get
      }

  def exists(settings: AppSettings): Boolean = stateFile(settings).exists()

  /**
   * Construct and returns the directory where state data will be stored
   * @param settings the configuration file for the node
   * @return a file where data is stored
   */
  def stateFile(settings: AppSettings): File = {
    val dataDir = settings.application.dataDir.ensuring(_.isDefined, "A data directory must be specified").get
    new File(s"$dataDir/state")
  }

  /**
   * @param settings
   * @param networkPrefix
   * @return
   */
  def readOrGenerate(settings: AppSettings)(implicit networkPrefix: NetworkPrefix): BoxState = {
    val sFile = stateFile(settings)
    sFile.mkdirs()
    val storage = new LDBKeyValueStore(new LDBVersionedStore(sFile, keepVersions = 100))

    apply(settings, storage, TokenBoxRegistry.readOrGenerate(settings), ProgramBoxRegistry.readOrGenerate(settings))
  }

  def apply(
    settings:               AppSettings,
    storage:                KeyValueStore,
    tokenBoxRegistry:       TokenBoxRegistry,
    programBoxRegistry:     ProgramBoxRegistry
  )(implicit networkPrefix: NetworkPrefix): BoxState = {
    val version: ModifierId =
      storage
        .latestVersionId()
        .fold(Option(ModifierId.empty))(bw => ModifierIdSerializer.parseBytes(bw).toOption)
        .getOrElse(throw new Error("Unable to define state version during initialization"))

    // node keys are a set of keys that this node will restrict its state to update
    val nodeKeys: Option[Set[Address]] = settings.application.nodeKeys match {
      case None                       => None
      case Some(keys) if keys.isEmpty => None
      case Some(keys)                 =>
        // decode keys into valid addresses
        (for {
          key <- keys
          decodedKeyOption =
            for {
              base58Key  <- Base58Data.validated(key).toOption
              keyAddress <- base58Key.decodeAddress.toOption
            } yield keyAddress
          decodedKey <- decodedKeyOption.toSet[Address]
        } yield decodedKey).some
    }

    if (nodeKeys.isDefined) log.info(s"Initializing state to watch for public keys: $nodeKeys")
    else log.info("Initializing state to watch for all public keys")

    BoxState(version, storage, tokenBoxRegistry, programBoxRegistry)
  }
}