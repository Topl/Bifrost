package bifrost.state

import java.io.File

import bifrost.crypto.{FastCryptographicHash, MultiSignature25519, PrivateKey25519, Signature25519}
import bifrost.exceptions.TransactionValidationException
import bifrost.history.History
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.modifier.box.serialization.{BoxSerializer, ExecutionBoxSerializer}
import bifrost.modifier.transaction.bifrostTransaction._
import bifrost.settings.AppSettings
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import com.google.common.primitives.Longs
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

/** BifrostState is a data structure which deterministically defines whether an arbitrary transaction is valid and so
  * applicable to it or not. Also has methods to get a closed box, to apply a persistent modifier, and to roll back
  * to a previous version.
  *
  * @param storage           singleton Iodb storage instance
  * @param version           blockId used to identify each block. Also used for rollback
  * @param timestamp         timestamp of the block that results in this state
  * @param history           Main box storage
  */
case class State(
  storage: LSMStore,
  override val version: VersionTag,
  timestamp: Long,
  history: History,
  pbr: ProgramBoxRegistry = null,
  tbr: TokenBoxRegistry = null,
  nodeKeys: Set[ByteArrayWrapper] = null
) extends MinimalState[Block, State]
    with Logging {

  override type NVCT = State
  type TX = Transaction
  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type BX = Box
  type BPMOD = Block
  type GSC = GenericStateChanges[Any, P, BX]
  type BSC = StateChanges

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

  def validate(transaction: TX): Try[Unit] = {
    transaction match {
      case tx: PolyTransfer           => validatePolyTransfer(tx)
      case tx: ArbitTransfer          => validateArbitTransfer(tx)
      case tx: AssetTransfer          => validateAssetTransfer(tx)
      case tx: ProgramTransfer        => validateProgramTransfer(tx)
      case tx: AssetCreation          => validateAssetCreation(tx)
      case tx: CodeCreation           => validateCodeCreation(tx)
      case tx: ProgramCreation        => validateProgramCreation(tx)
      case tx: ProgramMethodExecution => validateProgramMethodExecution(tx)
      case tx: CoinbaseTransaction    => validateCoinbaseTransaction(tx)
      case _ =>
        throw new Exception(
          "State validity not implemented for " + transaction.getClass.toGenericString
        )
    }
  }

  private def validateArbitTransfer(arT: ArbitTransfer): Try[Unit] = {

    val unlockers = generateUnlockers(arT.from, arT.signatures)

    val statefulValid: Try[Unit] = {
      val boxesSumTry: Try[Long] = {
        unlockers.foldLeft[Try[Long]](Success(0L))((partialRes, unlocker) =>
          partialRes.flatMap(partialSum =>
            /* Checks if unlocker is valid and if so adds to current running total */
            closedBox(unlocker.closedBoxId) match {
              case Some(box: ArbitBox) =>
                if (
                  unlocker.boxKey
                    .isValid(box.proposition, arT.messageToSign)
                ) {
                  Success(partialSum + box.value)
                } else {
                  Failure(new Exception("Incorrect unlocker"))
                }
              case None =>
                Failure(
                  new Exception(
                    s"Box for unlocker $unlocker is not in the state"
                  )
                )
              case _ =>
                Failure(new Exception("Invalid Box type for this transaction"))
            }
          )
        )
      }
      //Determine enough arbits
      boxesSumTry flatMap { openSum =>
        if (
          arT.newBoxes.map {
            case p: ArbitBox => p.value
            case _           => 0L
          }.sum == openSum - arT.fee
        ) {
          Success[Unit](Unit)
        } else {
          Failure(new Exception("Negative fee"))
        }
      }

    }

    statefulValid.flatMap(_ => State.syntacticValidity(arT))
  }

  /** @param poT : the PolyTransfer to validate
    * @return
    */
  private def validatePolyTransfer(poT: PolyTransfer): Try[Unit] = {

    val unlockers = generateUnlockers(poT.from, poT.signatures)

    val statefulValid: Try[Unit] = {
      val boxesSumTry: Try[Long] = {
        unlockers.foldLeft[Try[Long]](Success(0L))((partialRes, unlocker) =>
          partialRes.flatMap(partialSum =>
            /* Checks if unlocker is valid and if so adds to current running total */
            closedBox(unlocker.closedBoxId) match {
              case Some(box: PolyBox) =>
                if (
                  unlocker.boxKey
                    .isValid(box.proposition, poT.messageToSign)
                ) {
                  Success(partialSum + box.value)
                } else {
                  Failure(new Exception("Incorrect unlocker"))
                }
              case None =>
                Failure(
                  new Exception(
                    s"Box for unlocker $unlocker is not in the state"
                  )
                )
              case _ =>
                Failure(new Exception("Invalid Box type for this transaction"))
            }
          )
        )
      }
      determineEnoughPolys(boxesSumTry: Try[Long], poT)
    }

    statefulValid.flatMap(_ => State.syntacticValidity(poT))
  }

  private def validateAssetTransfer(asT: AssetTransfer): Try[Unit] = {

    val unlockers = generateUnlockers(asT.from, asT.signatures)

    val statefulValid: Try[Unit] = {
      val boxesSumTry: Try[Long] = {
        unlockers.foldLeft[Try[Long]](Success(0L))((partialRes, unlocker) =>
          partialRes
            .flatMap(partialSum =>
              /* Checks if unlocker is valid and if so adds to current running total */
              closedBox(unlocker.closedBoxId) match {
                case Some(box: AssetBox) =>
                  if (
                    unlocker.boxKey.isValid(
                      box.proposition,
                      asT.messageToSign
                    ) && (box.issuer equals asT.issuer) && (box.assetCode equals asT.assetCode)
                  ) {
                    Success(partialSum + box.value)
                  } else {
                    Failure(new Exception("Incorrect unlocker"))
                  }
                case None =>
                  Failure(
                    new Exception(
                      s"Box for unlocker $unlocker is not in the state"
                    )
                  )
                case _ =>
                  Failure(
                    new Exception("Invalid Box type for this transaction")
                  )
              }
            )
        )
      }
      determineEnoughAssets(boxesSumTry, asT)
    }

    statefulValid.flatMap(_ => State.syntacticValidity(asT))
  }

  private def validateProgramTransfer(prT: ProgramTransfer): Try[Unit] = {
    val from = Seq((prT.from, prT.executionBox.nonce))
    val signature = Map(prT.from -> prT.signature)
    val unlocker = generateUnlockers(from, signature).head

    val statefulValid: Try[Unit] = {
      prT.newBoxes.size match {
        case 1 =>
          if (prT.newBoxes.head.isInstanceOf[ExecutionBox])
            Success[Unit](Unit)
          else
            Failure(new Exception("Incorrect box type"))

        case _ => Failure(new Exception("Incorrect number of boxes created"))
      }

      closedBox(unlocker.closedBoxId) match {
        case Some(box: ExecutionBox) =>
          if (unlocker.boxKey.isValid(box.proposition, prT.messageToSign))
            Success[Unit](Unit)
          else
            Failure(new Exception("Incorrect unlocker"))

        case None =>
          Failure(
            new Exception(s"Box for unlocker $unlocker is not in the state")
          )
        case _ =>
          Failure(new Exception("Invalid Box type for this transaction"))
      }
    }

    statefulValid.flatMap(_ => State.syntacticValidity(prT))
  }

  private def validateAssetCreation(ac: AssetCreation): Try[Unit] = {
    val statefulValid: Try[Unit] = {
      ac.newBoxes.size match {
        //only one box should be created
        case 1 =>
          if (
            ac.newBoxes.head
              .isInstanceOf[AssetBox]
          ) // the new box is an asset box
            Success[Unit](Unit)
          else
            Failure(new Exception("Incorrect box type"))

        case _ => Failure(new Exception("Incorrect number of boxes created"))
      }
    }
    statefulValid.flatMap(_ => State.syntacticValidity(ac))
  }

  /** Check the code is valid chain code and the newly created CodeBox is
    * formed properly
    *
    * @param cc : CodeCreation object
    * @return
    */
  private def validateCodeCreation(cc: CodeCreation): Try[Unit] = {
    val statefulValid: Try[Unit] = {
      cc.newBoxes.size match {
        //only one box should be created
        case 1 =>
          if (cc.newBoxes.head.isInstanceOf[CodeBox])
            Success[Unit](Unit)
          else
            Failure(new Exception("Incorrect box type"))

        case _ => Failure(new Exception("Incorrect number of boxes created"))
      }
    }
    statefulValid.flatMap(_ => State.syntacticValidity(cc))
  }

  /** Validates ProgramCreation instance on its unlockers && timestamp of the program
    *
    * @param pc : ProgramCreation object
    * @return
    */
  private def validateProgramCreation(pc: ProgramCreation): Try[Unit] = {

    val unlockers = generateUnlockers(pc.boxIdsToOpen, pc.signatures.head._2)

    val unlockersValid: Try[Unit] = unlockers
      .foldLeft[Try[Unit]](Success(()))((unlockersValid, unlocker) =>
        unlockersValid
          .flatMap { _ =>
            closedBox(unlocker.closedBoxId) match {
              case Some(box) =>
                if (
                  unlocker.boxKey
                    .isValid(box.proposition, pc.messageToSign)
                ) {
                  Success(())
                } else {
                  Failure(
                    new TransactionValidationException("Incorrect unlocker")
                  )
                }
              case None =>
                Failure(
                  new TransactionValidationException(
                    s"Box for unlocker $unlocker is not in the state"
                  )
                )
            }
          }
      )

    val statefulValid = unlockersValid flatMap { _ =>
      val boxesAreNew = pc.newBoxes.forall(curBox =>
        storage.get(ByteArrayWrapper(curBox.id)) match {
          case Some(_) => false
          case None    => true
        }
      )

      if (boxesAreNew) {
        Success[Unit](Unit)
      } else {
        Failure(
          new TransactionValidationException(
            "ProgramCreation attempts to overwrite existing program"
          )
        )
      }
    }

    statefulValid.flatMap(_ => State.syntacticValidity(pc))
  }

  private def validateProgramMethodExecution(pme: ProgramMethodExecution): Try[Unit] = {
    //TODO get execution box from box registry using UUID before using its actual id to get it from storage
    val executionBoxBytes = storage.get(ByteArrayWrapper(pme.executionBox.id))

    /* Program exists */
    if (executionBoxBytes.isEmpty) {
      throw new TransactionValidationException(
        s"Program ${Base58.encode(pme.executionBox.id)} does not exist"
      )
    }

    val executionBox: ExecutionBox =
      ExecutionBoxSerializer.parseBytes(executionBoxBytes.get.data).get
    val programProposition: PublicKey25519Proposition = executionBox.proposition

    /* This person belongs to program */
    if (
      !MultiSignature25519(pme.signatures.values.toSet)
        .isValid(programProposition, pme.messageToSign)
    ) {
      throw new TransactionValidationException(
        s"Signature is invalid for ExecutionBox"
      )
    }

    val unlockers = generateUnlockers(pme.boxIdsToOpen, pme.signatures.head._2)

    val unlockersValid: Try[Unit] = unlockers
      .foldLeft[Try[Unit]](Success(()))((unlockersValid, unlocker) =>
        unlockersValid
          .flatMap { _ =>
            closedBox(unlocker.closedBoxId) match {
              case Some(box) =>
                if (
                  unlocker.boxKey
                    .isValid(box.proposition, pme.messageToSign)
                ) {
                  Success(())
                } else {
                  Failure(
                    new TransactionValidationException("Incorrect unlocker")
                  )
                }
              case None =>
                Failure(
                  new TransactionValidationException(
                    s"Box for unlocker $unlocker is not in the state"
                  )
                )
            }
          }
      )

    val statefulValid = unlockersValid flatMap { _ =>
      //Checks that newBoxes being created don't already exist
      val boxesAreNew = pme.newBoxes.forall(curBox =>
        storage.get(ByteArrayWrapper(curBox.id)) match {
          case Some(_) => false
          case None    => true
        }
      )

      if (boxesAreNew) {
        Success[Unit](Unit)
      } else {
        Failure(
          new TransactionValidationException(
            "ProgramCreation attempts to overwrite existing program"
          )
        )
      }
    }

    statefulValid.flatMap(_ => State.syntacticValidity(pme))
  }

  private def validateCoinbaseTransaction(cb: CoinbaseTransaction): Try[Unit] = {
    //val t = history.modifierById(cb.blockID).get
    //def helper(m: Block): Boolean = { m.id sameElements t.id }
    val validConstruction: Try[Unit] = {
      /*
      assert(cb.fee == 0L) // no fee for a coinbase tx
      assert(cb.newBoxes.size == 1) // one one new box
      assert(cb.newBoxes.head.isInstanceOf[ArbitBox]) // the new box is an arbit box
       This will be implemented at a consensus level
       */
      Try {
        // assert(cb.newBoxes.head.asInstanceOf[ArbitBox].value == history.modifierById(history.chainBack(history.bestBlock, helper).get.reverse(1)._2).get.inflation)
      }
    }
    validConstruction

  }

  private def determineEnoughAssets(boxesSumTry: Try[Long], tx: Transaction): Try[Unit] = {
    boxesSumTry flatMap { openSum =>
      if (
        tx.newBoxes.map {
          case a: AssetBox => a.value
          case _           => 0L
        }.sum <= openSum
      ) {
        Success[Unit](Unit)
      } else {
        Failure(new Exception("Not enough assets"))
      }
    }
  }

  private def determineEnoughPolys(boxesSumTry: Try[Long], tx: Transaction): Try[Unit] = {
    boxesSumTry flatMap { openSum =>
      if (
        tx.newBoxes.map {
          case p: PolyBox => p.value
          case _          => 0L
        }.sum == openSum - tx.fee
      ) {
        Success[Unit](Unit)
      } else {
        Failure(new Exception("Negative fee"))
      }
    }
  }

  def closedBox(boxId: Array[Byte]): Option[BX] =
    storage
      .get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(BoxSerializer.parseBytes)
      .flatMap(_.toOption)

  private def generateUnlockers(
    from: Seq[(PublicKey25519Proposition, Transaction.Nonce)],
    signatures: Map[PublicKey25519Proposition, Signature25519]
  ): Traversable[BoxUnlocker[PublicKey25519Proposition]] = {
    from.map { case (prop, nonce) =>
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

  private def generateUnlockers(
    boxIds: Seq[Array[Byte]],
    signature: Signature25519
  ): Traversable[BoxUnlocker[PublicKey25519Proposition]] = {
    boxIds.map { id =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = id
        override val boxKey: Signature25519 = signature
      }
    }
  }

  override def applyModifier(mod: BPMOD): Try[State] =
    mod match {
      case b: Block ⇒
        StateChanges(b).flatMap(cs ⇒ applyChanges(cs, b.id))
      //case a: Any ⇒
      // Failure(new Exception(s"unknown modifier $a"))
    }

  // not private because of tests
  def applyChanges(changes: GSC, newVersion: VersionTag): Try[NVCT] =
    Try {

      //Filtering boxes pertaining to public keys specified in settings file
      //Note YT - Need to handle MofN Proposition separately
      val keyFilteredBoxesToAdd =
        if (nodeKeys != null)
          changes.toAppend
            .filter(b => nodeKeys.contains(ByteArrayWrapper(b.proposition.bytes)))
        else
          changes.toAppend

      val keyFilteredBoxIdsToRemove =
        if (nodeKeys != null)
          changes.boxIdsToRemove
            .flatMap(closedBox)
            .filter(b => nodeKeys.contains(ByteArrayWrapper(b.proposition.bytes)))
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

  // todo: JAA - shouldn't this be read in from a settings file or is it even needed (can we remove it from the trait?)
  override def maxRollbackDepth: Int = 10
}

object State extends Logging {

  def genesisState(settings: AppSettings, initialBlocks: Seq[Block], history: History): State = {
    initialBlocks
      .foldLeft(readOrGenerate(settings, callFromGenesis = true, history)) { (state, mod) =>
        StateChanges(mod)
          .flatMap(cs => state.applyChanges(cs, mod.id))
          .get
      }
  }

  /** Provides a single interface for syntactically validating transactions
    *
    * @param tx transaction to evaluate
    */
  def syntacticValidity[TX](tx: TX): Try[Unit] = {
    tx match {
      case tx: PolyTransfer           => PolyTransfer.validate(tx)
      case tx: ArbitTransfer          => ArbitTransfer.validate(tx)
      case tx: AssetTransfer          => AssetTransfer.validate(tx)
      case tx: ProgramTransfer        => ProgramTransfer.validate(tx)
      case tx: AssetCreation          => AssetCreation.validate(tx)
      case tx: CodeCreation           => CodeCreation.validate(tx)
      case tx: ProgramCreation        => ProgramCreation.validate(tx)
      case tx: ProgramMethodExecution => ProgramMethodExecution.validate(tx)
      case tx: CoinbaseTransaction    => CoinbaseTransaction.validate(tx)
      case _ =>
        throw new UnsupportedOperationException(
          "Semantic validity not implemented for " + tx.getClass.toGenericString
        )
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

    State(stateStorage, version, timestamp, history, pbr, tbr, nodeKeys)
  }
}
