package bifrost.state

import java.io.File
import java.util.UUID

import akka.actor.Status.Success
import bifrost.crypto.{ FastCryptographicHash, PrivateKey25519, Signature25519 }
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

import scala.util.{ Failure, Try }

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
                   private val storage : LSMStore,
                   private val tbrOpt  : Option[TokenBoxRegistry] = None,
                   private val pbrOpt  : Option[ProgramBoxRegistry] = None,
                   nodeKeys            : Option[Set[ByteArrayWrapper]] = None
                 ) extends MinimalState[Any, ProofOfKnowledgeProposition[PrivateKey25519], Box, Block, State]
                           with StoreInterface
                           with TransactionValidation[Transaction]
                           with Logging {

  override type NVCT = State
  type BSC = StateChanges

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
        case k: ProgramBoxRegistry.K if pbrOpt.isDefined => Some(pbrOpt.get.lookup(k))
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
  override def applyModifier ( mod: Block ): Try[State] =
    mod match {
      case b: Block ⇒ StateChanges(b).flatMap(sc ⇒ applyChanges(sc, b.id))
      case a: Any   ⇒ Failure(new Exception(s"unknown modifier $a"))
    }

  // not private because of tests
  def applyChanges ( changes: BSC, newVersion: VersionTag ): Try[NVCT] =
    Try {

      //Filtering boxes pertaining to public keys specified in settings file
      val boxesToAdd = (nodeKeys match {
        case Some(keys) => changes.toAppend.filter(b => keys.contains(ByteArrayWrapper(b.proposition.bytes)))
        case None       => changes.toAppend
      }).map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

      val boxIdsToRemove = (nodeKeys match {
        case Some(keys) => changes.boxIdsToRemove
                                  .flatMap(getBox)
                                  .filter(b => keys.contains(ByteArrayWrapper(b.proposition.bytes)))
                                  .map(b => b.id)

        case None       => changes.boxIdsToRemove
      }).map(b => ByteArrayWrapper(b))

      // enforce that the input id's must not match any of the output id's
      require(!boxesToAdd.forall(b => boxIdsToRemove.contains(b._1)), s"Attempted application of invalid state")

      log.debug(
        s"Update BifrostState from version ${version.toString} to version $newVersion. " +
          s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
          s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}"
        )

      if ( storage.lastVersionID.isDefined ) boxIdsToRemove.foreach(i => require(getBox(i.data).isDefined))

      //TokenBoxRegistry must be updated before state since it uses the boxes from state that are being removed in the update
      if ( tbrOpt.isDefined ) tbrOpt.get.updateFromState(newVersion, boxIdsToRemove, boxesToAdd)
      if ( pbrOpt.isDefined ) pbrOpt.get.updateFromState(newVersion, boxIdsToRemove, boxesToAdd)

      storage.update(ByteArrayWrapper(newVersion.hashBytes), boxIdsToRemove, boxesToAdd)

      val newSt = State(newVersion, storage, tbrOpt, pbrOpt, nodeKeys)

      // enforce that a new valid state must have emptied all boxes to remove
      boxIdsToRemove.foreach(box => require(newSt.getBox(box.data).isEmpty, s"Box $box is still in state"))

      newSt
    }

  def validate[TX] ( transaction: TX ): Try[Unit] = {
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
        ( state, mod ) =>
          StateChanges(mod)
            .flatMap(cs => state.applyChanges(cs, mod.id))
            .get
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
    val dataDirOpt = settings.dataDir.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

    new File(dataDir).mkdirs()

    val iFile = new File(s"$dataDir/state")
    iFile.mkdirs()
    val stateStorage = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run ( ): Unit = {
        stateStorage.close()
      }
    })

    val version: VersionTag = ModifierId(
      stateStorage.lastVersionID
        .fold(Array.emptyByteArray)(_.data)
      )

    //TODO clean up nulls
    //TODO fix bug where walletSeed and empty nodeKeys setting prevents forging - JAA
    val nodeKeys: Set[ByteArrayWrapper] = settings.nodeKeys
      .map(x => x.map(y => ByteArrayWrapper(Base58.decode(y).get)))
      .orNull
    val pbr = ProgramBoxRegistry.readOrGenerate(settings, stateStorage).orNull
    val tbr = TokenBoxRegistry.readOrGenerate(settings, stateStorage).orNull
    if ( pbr == null ) log.info("Initializing state without programBoxRegistry")
    else log.info("Initializing state with programBoxRegistry")
    if ( tbr == null ) log.info("Initializing state without tokenBoxRegistry")
    else log.info("Initializing state with tokenBoxRegistry")
    if ( nodeKeys != null )
      log.info(s"Initializing state to watch for public keys: ${
        nodeKeys
          .map(x => Base58.encode(x.data))
      }")
    else log.info("Initializing state to watch for all public keys")

    State(stateStorage, version, pbr, tbr, nodeKeys)
  }
}
