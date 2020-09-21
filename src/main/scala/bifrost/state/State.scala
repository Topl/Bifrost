package bifrost.state

import java.io.File

import bifrost.crypto.{FastCryptographicHash, PrivateKey25519, Signature25519}
import bifrost.history.History
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.settings.AppSettings
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import com.google.common.primitives.Longs
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.crypto.encode.Base58

import scala.util.{Failure, Try}

/**
  * BifrostState is a data structure which deterministically defines whether an arbitrary transaction is valid and so
  * applicable to it or not. Also has methods to get a closed box, to apply a persistent modifier, and to roll back
  * to a previous version.
  *
  * @param storage           singleton Iodb storage instance
  * @param version           blockId used to identify each block. Also used for rollback
  * @param timestamp         timestamp of the block that results in this state
  * @param history           Main box storage
  */
case class State( storage: LSMStore,
                  override val version: VersionTag,
                  timestamp: Long,
                  history: History,
                  pbr: ProgramBoxRegistry = null,
                  tbr: TokenBoxRegistry = null,
                  nodeKeys: Set[ByteArrayWrapper] = null
                ) extends MinimalState[Block, State]
                          with TransactionValidation[Transaction]
                          with StateReader[Box, ProofOfKnowledgeProposition[PrivateKey25519], Any]
                          with Logging {

  override type NVCT = State

  type TX = Transaction
  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type BX = Box
  type BPMOD = Block
  type GSC = GenericStateChanges[Any, P, BX]
  type BSC = StateChanges

  def getReader: StateReader[Box, ProofOfKnowledgeProposition[PrivateKey25519], Any] = this

  def validate(transaction: TX): Try[Unit] = {
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
      case _ =>
        throw new Exception(
          "State validity not implemented for " + transaction.getClass.toGenericString
        )
    }
  }

  override def rollbackTo(version: VersionTag): Try[NVCT] =
    Try {
      if (storage.lastVersionID.exists(_.data sameElements version.hashBytes)) {
        this
      } else {
        log.debug(
          s"Rollback BifrostState to $version from version $lastVersionString"
        )
        storage.rollback(ByteArrayWrapper(version.hashBytes))
        tbr.rollbackTo(version, storage)
        pbr.rollbackTo(version, storage)
        val timestamp: Long = Longs.fromByteArray(
          storage
            .get(ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes)))
            .get
            .data
        )
        State(storage, version, timestamp, history, pbr, tbr)
      }
    }

  private def lastVersionString: String =
    storage.lastVersionID.map(v => Base58.encode(v.data)).getOrElse("None")

  def closedBox(id: Array[Byte]): Option[BX] =
    storage
      .get(ByteArrayWrapper(id))
      .map(_.data)
      .map(BoxSerializer.parseBytes)
      .flatMap(_.toOption)

  override def applyModifier(mod: BPMOD): Try[State] =
    mod match {
      case b: Block ⇒ StateChanges(b).flatMap(sc ⇒ applyChanges(sc, b.id))
      case a: Any   ⇒ Failure(new Exception(s"unknown modifier $a"))
    }

  // not private because of tests
  def applyChanges(changes: GSC, newVersion: VersionTag): Try[NVCT] =
    Try {

      //Filtering boxes pertaining to public keys specified in settings file
      //Note YT - Need to handle MofN Proposition separately
      val keyFilteredBoxesToAdd =
        if (nodeKeys != null)
          changes.toAppend
            .filter(b =>
              nodeKeys.contains(ByteArrayWrapper(b.proposition.bytes))
            )
        else
          changes.toAppend

      val keyFilteredBoxIdsToRemove =
        if (nodeKeys != null)
          changes.boxIdsToRemove
            .flatMap(closedBox)
            .filter(b =>
              nodeKeys.contains(ByteArrayWrapper(b.proposition.bytes))
            )
            .map(b => b.id)
        else
          changes.boxIdsToRemove

      val boxesToAdd = keyFilteredBoxesToAdd
        .map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

      /* This seeks to avoid the scenario where there is remove and then update of the same keys */
      val boxIdsToRemove =
        (keyFilteredBoxIdsToRemove -- boxesToAdd.map(_._1.data))
          .map(ByteArrayWrapper.apply)

      log.debug(
        s"Update BifrostState from version $lastVersionString to version $newVersion. " +
          s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
          s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}"
      )

      val timestamp: Long = changes.asInstanceOf[StateChanges].timestamp

      if (storage.lastVersionID.isDefined)
        boxIdsToRemove.foreach(i => require(closedBox(i.data).isDefined))

      //TokenBoxRegistry must be updated before state since it uses the boxes from state that are being removed in the update
      if (tbr != null)
        tbr.updateFromState(
          newVersion,
          keyFilteredBoxIdsToRemove,
          keyFilteredBoxesToAdd
        )
      if (pbr != null)
        pbr.updateFromState(
          newVersion,
          keyFilteredBoxIdsToRemove,
          keyFilteredBoxesToAdd
        )

      storage.update(
        ByteArrayWrapper(newVersion.hashBytes),
        boxIdsToRemove,
        boxesToAdd + (ByteArrayWrapper(
          FastCryptographicHash("timestamp".getBytes)
        ) -> ByteArrayWrapper(Longs.toByteArray(timestamp)))
      )

      val newSt =
        State(storage, newVersion, timestamp, history, pbr, tbr, nodeKeys)

      boxIdsToRemove.foreach(box =>
        require(
          newSt.closedBox(box.data).isEmpty,
          s"Box $box is still in state"
        )
      )
      newSt
    }
}

object State extends Logging {

  def genesisState(settings: AppSettings, initialBlocks: Seq[Block], history: History): State = {
    initialBlocks
      .foldLeft(readOrGenerate( settings, callFromGenesis = true, history)) {
        (state, mod) =>
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
  def syntacticValidity[TX](tx: TX): Try[Unit] = {
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
      case _ =>
        throw new UnsupportedOperationException(
          "Semantic validity not implemented for " + tx.getClass.toGenericString
        )
    }
  }

  private[transaction] def generateUnlockers( from: Seq[(PublicKey25519Proposition, Transaction.Nonce)],
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

  private[transaction] def generateUnlockers( boxIds: Seq[Array[Byte]],
                                              signature: Signature25519
                                            ): Traversable[BoxUnlocker[PublicKey25519Proposition]] = {
    boxIds.map { id =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = id
        override val boxKey: Signature25519 = signature
      }
    }
  }

  def readOrGenerate(settings: AppSettings, callFromGenesis: Boolean = false, history: History): State = {
    val dataDirOpt = settings.dataDir.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

    new File(dataDir).mkdirs()

    val iFile = new File(s"$dataDir/state")
    iFile.mkdirs()
    val stateStorage = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        stateStorage.close()
      }
    })

    val version: VersionTag = ModifierId(
      stateStorage.lastVersionID
        .fold(Array.emptyByteArray)(_.data)
    )

    val timestamp: Long = if (callFromGenesis) {
      System.currentTimeMillis()
    } else {
      Longs.fromByteArray(
        stateStorage
          .get(ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes)))
          .get
          .data
      )
    }

    //TODO clean up nulls
    //TODO fix bug where walletSeed and empty nodeKeys setting prevents forging - JAA
    val nodeKeys: Set[ByteArrayWrapper] = settings.nodeKeys
      .map(x => x.map(y => ByteArrayWrapper(Base58.decode(y).get)))
      .orNull
    val pbr = ProgramBoxRegistry.readOrGenerate(settings, stateStorage).orNull
    val tbr = TokenBoxRegistry.readOrGenerate(settings, stateStorage).orNull
    if (pbr == null) log.info("Initializing state without programBoxRegistry")
    else log.info("Initializing state with programBoxRegistry")
    if (tbr == null) log.info("Initializing state without tokenBoxRegistry")
    else log.info("Initializing state with tokenBoxRegistry")
    if (nodeKeys != null)
      log.info(s"Initializing state to watch for public keys: ${nodeKeys
        .map(x => Base58.encode(x.data))}")
    else log.info("Initializing state to watch for all public keys")

    State(stateStorage, version, timestamp, history, pbr, tbr, nodeKeys )
  }
}
