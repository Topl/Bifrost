package bifrost.state

import java.io.File

import bifrost.crypto.{ PrivateKey25519, Signature25519 }
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{ ProofOfKnowledgeProposition, PublicKey25519Proposition }
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.settings.AppSettings
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import io.iohk.iodb.{ ByteArrayWrapper, LSMStore }
import scorex.crypto.encode.Base58

import scala.util.{ Failure, Success, Try }

/**
 * BifrostState is a data structure which deterministically defines whether an arbitrary transaction is valid and so
 * applicable to it or not. Also has methods to get a closed box, to apply a persistent modifier, and to roll back
 * to a previous version.
 *
 * @param storage   singleton Iodb storage instance
 * @param version   blockId used to identify each block. Also used for rollback
 * //@param timestamp timestamp of the block that results in this state
 */
case class State ( override val version: VersionTag,
                   protected val storage : LSMStore,
                   private val tbrOpt  : Option[TokenBoxRegistry] = None,
                   private val pbrOpt  : Option[ProgramBoxRegistry] = None,
                   nodeKeys            : Option[Set[PublicKey25519Proposition]] = None
                 ) extends MinimalState[Any, ProofOfKnowledgeProposition[PrivateKey25519], Box, Block, State]
                           with StoreInterface
                           with TransactionValidation[Transaction]
                           with Logging {

  override type NVCT = State

  override def closeStorage(): Unit = {
    log.info("Attempting to close state storage")
    super.closeStorage()

    log.info("Attempting to close token box registry storage")
    tbrOpt.foreach(_.closeStorage())

    log.info("Attempting to close program box registry storage")
    pbrOpt.foreach(_.closeStorage())
  }

  def getBox ( id: Array[Byte] ): Option[Box] =
    getFromStorage(id)
      .map(BoxSerializer.parseBytes)
      .flatMap(_.toOption)


  /**
   * Retrieve a sequence of token boxes from the UTXO set based on the given key
   *
   * @param key storage key used to identify value(s) in registry
   * @return a sequence of boxes stored beneath the specified key
   */
  def registryLookup[K](key: K): Option[Seq[Box]] = {
      (key match {
        case k: TokenBoxRegistry.K   if tbrOpt.isDefined => Some(tbrOpt.get.lookup(k))
        case k: ProgramBoxRegistry.K if pbrOpt.isDefined => Some(pbrOpt.get.lookup(k).map(_.hashBytes))
        case _ if pbrOpt.isEmpty | tbrOpt.isEmpty        => None
      }).map { _.map(getBox).filter {
          case _: Some[Box] => true
          case None         => false
        }.map(_.get)
      }
  }

  /**
   * Revert version of state to a specific version
   *
   * @param version tag marking a specific state within the store
   * @return an instance of State at the version given
   */
  override def rollbackTo ( version: VersionTag ): Try[NVCT] =
    Try {
      if ( storage.lastVersionID.exists(_.data sameElements version.hashBytes) ) {
        this
      } else {
        log.debug(s"Rollback BifrostState to $version from version ${this.version.toString}")
        storage.rollback(ByteArrayWrapper(version.hashBytes))
        if (tbrOpt.isDefined) tbrOpt.get.rollbackTo(version)
        if (pbrOpt.isDefined) pbrOpt.get.rollbackTo(version)

        State(version, storage, tbrOpt, pbrOpt, nodeKeys)
      }
    }

  /**
   * Public method used to update an instance of state with a new set of transactions
   *
   * @param mod block to be applied to state
   * @return a new instance of state with the transaction within mod applied
   */
  override def applyModifier ( mod: Block ): Try[State] = {

    // extract the state changes to be made, using option for TBR and PBR
    // since we can skip if the registries aren't present
    mod match {
      case b: Block ⇒
        val stateChanges = StateChanges(b)
        val tokenChanges = if ( tbrOpt.isDefined ) TokenRegistryChanges(b).toOption else None
        val programChanges = if ( pbrOpt.isDefined ) ProgramRegistryChanges(b).toOption else None

        // throwing error here since we should stop attempting updates if any part fails
        val updatedTBR = tokenChanges match {
          case Some(tc) => tbrOpt.get.update(b.id, tc.toRemove, tc.toAppend) match {
            case Success(updates) => Some(updates)
            case Failure(ex)      => throw new Error(s"Failed to update TBR with error $ex")
          }
          case None => None
        }

        val updatedPBR = programChanges match {
          case Some(pc) => pbrOpt.get.update(b.id, pc.toRemove, pc.toUpdate) match {
            case Success(updates) => Some(updates)
            case Failure(ex)      => throw new Error(s"Failed to update PBR with error $ex")
          }
          case None => None
        }

        // if the registries update successfully, attempt to update the utxo storage
        stateChanges match {
          case Success(sc) => applyChanges(b.id, sc, updatedTBR, updatedPBR)
          case Failure(_)  => throw new Error(s"Failed to calculate state changes for block: ${b.id}")
        }

      case a: Any ⇒ Failure(new Exception(s"unknown modifier $a"))
    }
  }

  // not private because of tests
  def applyChanges ( newVersion: VersionTag,
                     stateChanges: StateChanges,
                     newTBR: Option[TokenBoxRegistry],
                     newPBR: Option[ProgramBoxRegistry]
                   ): Try[NVCT] =
    Try {

      //Filtering boxes pertaining to public keys specified in settings file
      val boxesToAdd = (nodeKeys match {
        case Some(keys) => stateChanges.toAppend.filter(b => keys.contains(PublicKey25519Proposition(b.proposition.bytes)))
        case None       => stateChanges.toAppend
      }).map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

      val boxIdsToRemove = (nodeKeys match {
        case Some(keys) => stateChanges.boxIdsToRemove
                                  .flatMap(getBox)
                                  .filter(b => keys.contains(PublicKey25519Proposition(b.proposition.bytes)))
                                  .map(b => b.id)

        case None       => stateChanges.boxIdsToRemove
      }).map(b => ByteArrayWrapper(b))

      // enforce that the input id's must not match any of the output id's
      require(!boxesToAdd.forall(b => boxIdsToRemove.contains(b._1)), s"Attempted application of invalid state")

      log.debug(
        s"Update BifrostState from version ${version.toString} to version $newVersion. " +
          s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
          s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}"
        )

      if ( storage.lastVersionID.isDefined ) boxIdsToRemove.foreach(id => require(getBox(id.data).isDefined))
      storage.update(ByteArrayWrapper(newVersion.hashBytes), boxIdsToRemove, boxesToAdd)

      // create updated instance of state
      val newState = State(newVersion, storage, newTBR, newPBR, nodeKeys)

      // enforce that a new valid state must have emptied all boxes to remove
      boxIdsToRemove.foreach(box => require(newState.getBox(box.data).isEmpty, s"Box $box is still in state"))

      newState
    }

  def validate ( transaction: Transaction ): Try[Unit] = {
    transaction match {
      case tx: CoinbaseTransaction    => CoinbaseTransaction.semanticValidate(tx, getReader)
      case tx: ArbitTransfer          => ArbitTransfer.semanticValidate(tx, getReader)
      case tx: PolyTransfer           => PolyTransfer.semanticValidate(tx, getReader)
      case tx: AssetTransfer          => AssetTransfer.semanticValidate(tx, getReader)
      case tx: ProgramTransfer        => ProgramTransfer.semanticValidate(tx, getReader)
      case tx: AssetCreation          => AssetCreation.semanticValidate(tx, getReader)
      case tx: CodeCreation           => CodeCreation.semanticValidate(tx, getReader)
      case tx: ProgramCreation        => ProgramCreation.semanticValidate(tx, getReader)
      case tx: ProgramMethodExecution => ProgramMethodExecution.semanticValidate(tx, getReader)
      case _                          =>
        throw new Exception(
          "State validity not implemented for " + transaction.getClass.toGenericString
          )
    }
  }
}

object State extends Logging {

  def genesisState ( settings: AppSettings, initialBlocks: Seq[Block] ): State = {
    initialBlocks
      .foldLeft(readOrGenerate(settings, callFromGenesis = true)) {
        ( state, mod ) => state.applyModifier(mod).get
      }
  }

  /**
   * Provides a single interface for syntactically validating transactions
   *
   * @param tx transaction to evaluate
   */
  def syntacticValidity[TX] ( tx: TX ): Try[Unit] = {
    tx match {
      case tx: PolyTransfer           => PolyTransfer.syntacticValidate(tx)
      case tx: ArbitTransfer          => ArbitTransfer.syntacticValidate(tx)
      case tx: AssetTransfer          => AssetTransfer.syntacticValidate(tx)
      case tx: ProgramTransfer        => ProgramTransfer.syntacticValidate(tx)
      case tx: AssetCreation          => AssetCreation.syntacticValidate(tx)
      case tx: CodeCreation           => CodeCreation.syntacticValidate(tx)
      case tx: ProgramCreation        => ProgramCreation.syntacticValidate(tx)
      case tx: ProgramMethodExecution => ProgramMethodExecution.syntacticValidate(tx)
      case tx: CoinbaseTransaction    => CoinbaseTransaction.syntacticValidate(tx)
      case _                          =>
        throw new UnsupportedOperationException(
          "Semantic validity not implemented for " + tx.getClass.toGenericString
          )
    }
  }

  private[transaction] def generateUnlockers ( from      : Seq[(PublicKey25519Proposition, Transaction.Nonce)],
                                               signatures: Map[PublicKey25519Proposition, Signature25519]
                                             ): Traversable[BoxUnlocker[PublicKey25519Proposition]] = {
    from.map {
      case (prop, nonce) =>
        new BoxUnlocker[PublicKey25519Proposition] {
          override val closedBoxId: Array[Byte] =
            PublicKeyNoncedBox.idFromBox(prop, nonce)
          override val boxKey: Signature25519 = signatures.getOrElse(
            prop,
            throw new Exception("Signature not provided")
            )
        }
    }
  }

  private[transaction] def generateUnlockers ( boxIds   : Seq[Array[Byte]],
                                               signature: Signature25519
                                             ): Traversable[BoxUnlocker[PublicKey25519Proposition]] = {
    boxIds.map { id =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = id
        override val boxKey: Signature25519 = signature
      }
    }
  }

  def readOrGenerate ( settings: AppSettings, callFromGenesis: Boolean = false ): State = {
    val dataDir = settings.dataDir.ensuring(_.isDefined, "data dir must be specified").get

    new File(dataDir).mkdirs()

    val iFile = new File(s"$dataDir/state")
    iFile.mkdirs()
    val storage = new LSMStore(iFile)

    val version: VersionTag = ModifierId(
      storage.lastVersionID
        .fold(Array.emptyByteArray)(_.data)
      )

    // node keys are a set of keys that this node will restrict its state to update
    val nodeKeys: Option[Set[PublicKey25519Proposition]] =
      settings
        .nodeKeys
        .map(_.map(key => PublicKey25519Proposition(Base58.decode(key).get)))

    if ( nodeKeys.isDefined )
      log.info(s"Initializing state to watch for public keys: ${
        nodeKeys
          .get
          .map(x => Base58.encode(x.bytes))
      }")
    else log.info("Initializing state to watch for all public keys")

    //TODO fix bug where walletSeed and empty nodeKeys setting prevents forging - JAA
    val pbr = ProgramBoxRegistry.readOrGenerate(settings)
    val tbr = TokenBoxRegistry.readOrGenerate(settings, nodeKeys)

    if ( pbr.isEmpty ) log.info("Initializing state without programBoxRegistry")
    else log.info("Initializing state with programBoxRegistry")

    if ( tbr.isEmpty ) log.info("Initializing state without tokenBoxRegistry")
    else log.info("Initializing state with tokenBoxRegistry")



    State(version, storage, tbr, pbr, nodeKeys)
  }
}
