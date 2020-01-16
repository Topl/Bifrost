package bifrost.state

import java.io.File

import bifrost.tokenBoxRegistry.TokenBoxRegistry
import bifrost.history.BifrostHistory
import bifrost.blocks.BifrostBlock
import bifrost.exceptions.TransactionValidationException
import bifrost.scorexMod.{GenericBoxMinimalState, GenericStateChanges}
import bifrost.transaction.box._
import bifrost.transaction.proof.{MultiSignature25519, Signature25519}
import com.google.common.primitives.Longs
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.forging.ForgingSettings
import bifrost.programBoxRegistry.ProgramBoxRegistry
import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.bifrostTransaction.{AssetRedemption, _}
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.state.MinimalState.VersionTag
import bifrost.transaction.state.PrivateKey25519
import bifrost.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

case class BifrostTransactionChanges(toRemove: Set[BifrostBox], toAppend: Set[BifrostBox], minerReward: Long)

case class BifrostStateChanges(override val boxIdsToRemove: Set[Array[Byte]],
                               override val toAppend: Set[BifrostBox], timestamp: Long)
  extends GenericStateChanges[Any, ProofOfKnowledgeProposition[PrivateKey25519], BifrostBox](boxIdsToRemove, toAppend)

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
//noinspection ScalaStyle
case class BifrostState(storage: LSMStore, override val version: VersionTag, timestamp: Long, history: BifrostHistory, pbr: ProgramBoxRegistry = null, tbr: TokenBoxRegistry = null, nodeKeys: Set[ByteArrayWrapper] = null)
  extends GenericBoxMinimalState[Any, ProofOfKnowledgeProposition[PrivateKey25519],
    BifrostBox, BifrostTransaction, BifrostBlock, BifrostState] with ScorexLogging {

  override type NVCT = BifrostState
  type P = BifrostState.P
  type T = BifrostState.T
  type TX = BifrostState.TX
  type BX = BifrostState.BX
  type BPMOD = BifrostState.BPMOD
  type GSC = BifrostState.GSC
  type BSC = BifrostState.BSC



  override def semanticValidity(tx: BifrostTransaction): Try[Unit] = BifrostState.semanticValidity(tx)

  private def lastVersionString = storage.lastVersionID.map(v => Base58.encode(v.data)).getOrElse("None")

  override def closedBox(boxId: Array[Byte]): Option[BX] =
    storage.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(BifrostBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  override def rollbackTo(version: VersionTag): Try[NVCT] = Try {
    if (storage.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rollback BifrostState to ${Base58.encode(version)} from version $lastVersionString")
      storage.rollback(ByteArrayWrapper(version))
      tbr.rollbackTo(version, storage)
      pbr.rollbackTo(version, storage)
      val timestamp: Long = Longs.fromByteArray(storage.get(ByteArrayWrapper(FastCryptographicHash("timestamp"
        .getBytes))).get
        .data)
      BifrostState(storage, version, timestamp, history, pbr, tbr)
    }
  }

  override def changes(mod: BPMOD): Try[GSC] = BifrostState.changes(mod)


  override def applyChanges(changes: GSC, newVersion: VersionTag): Try[NVCT] = Try {

    //Filtering boxes pertaining to public keys specified in settings file
    //Note YT - Need to handle MofN Proposition separately
    val keyFilteredBoxesToAdd =
      if(nodeKeys != null)
        changes.toAppend
          .filter(b => nodeKeys.contains(ByteArrayWrapper(b.proposition.bytes)))
      else
        changes.toAppend

    val keyFilteredBoxIdsToRemove =
      if(nodeKeys != null)
        changes.boxIdsToRemove
        .flatMap(closedBox)
        .filter(b => nodeKeys.contains(ByteArrayWrapper(b.proposition.bytes)))
        .map(b => b.id)
      else
        changes.boxIdsToRemove

    val boxesToAdd = keyFilteredBoxesToAdd
      .map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    /* This seeks to avoid the scenario where there is remove and then update of the same keys */
    val boxIdsToRemove = (keyFilteredBoxIdsToRemove -- boxesToAdd.map(_._1.data)).map(ByteArrayWrapper.apply)

    log.debug(s"Update BifrostState from version $lastVersionString to version ${Base58.encode(newVersion)}. " +
      s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
      s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}")

    val timestamp: Long = changes.asInstanceOf[BifrostStateChanges].timestamp

    if (storage.lastVersionID.isDefined) boxIdsToRemove.foreach(i => require(closedBox(i.data).isDefined))

    //TokenBoxRegistry must be updated before state since it uses the boxes from state that are being removed in the update
    if(tbr != null) tbr.updateFromState(newVersion, keyFilteredBoxIdsToRemove, keyFilteredBoxesToAdd)
    if(pbr != null) pbr.updateFromState(newVersion, keyFilteredBoxIdsToRemove, keyFilteredBoxesToAdd)


    storage.update(
      ByteArrayWrapper(newVersion),
      boxIdsToRemove,
      boxesToAdd + (ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes)) -> ByteArrayWrapper(Longs.toByteArray(
        timestamp)))
    )

    val newSt = BifrostState(storage, newVersion, timestamp, history, pbr, tbr, nodeKeys)

    boxIdsToRemove.foreach(box => require(newSt.closedBox(box.data).isEmpty, s"Box $box is still in state"))
    newSt
  }



  //noinspection ScalaStyle
  override def validate(transaction: TX): Try[Unit] = {
    transaction match {
      case poT: PolyTransfer => validatePolyTransfer(poT)
      case arT: ArbitTransfer => validateArbitTransfer(arT)
      case asT: AssetTransfer => validateAssetTransfer(asT)
      case prT: ProgramTransfer => validateProgramTransfer(prT)
      case ac: AssetCreation => validateAssetCreation(ac)
      case cc: CodeCreation => validateCodeCreation(cc)
      case pc: ProgramCreation => validateProgramCreation(pc)
      case cme: ProgramMethodExecution => validateProgramMethodExecution(cme)
      case ar: AssetRedemption => validateAssetRedemption(ar)
      case cb: CoinbaseTransaction => validateCoinbaseTransaction(cb)
      case _ => throw new Exception("State validity not implemented for " + transaction.getClass.toGenericString)
    }
  }

  /**
    *
    * @param poT : the PolyTransfer to validate
    * @return
    */
  def validatePolyTransfer(poT: PolyTransfer): Try[Unit] = {

    val unlockers = generateUnlockers(poT.from, poT.signatures)

    val statefulValid: Try[Unit] = {
      val boxesSumTry: Try[Long] = {
        unlockers.foldLeft[Try[Long]](Success(0L))((partialRes, unlocker) =>

          partialRes.flatMap(partialSum =>
            /* Checks if unlocker is valid and if so adds to current running total */
            closedBox(unlocker.closedBoxId) match {
              case Some(box: PolyBox) =>
                if (unlocker.boxKey.isValid(box
                  .proposition,
                  poT
                    .messageToSign)) {
                  Success(partialSum + box.value)
                } else {
                  Failure(new Exception(
                    "Incorrect unlocker"))
                }
              case None => Failure(new Exception(s"Box for unlocker $unlocker is not in the state"))
            }
          )
        )
      }
      determineEnoughPolys(boxesSumTry: Try[Long], poT)
    }

    statefulValid.flatMap(_ => semanticValidity(poT))
  }

  private def determineEnoughPolys(boxesSumTry: Try[Long], tx: BifrostTransaction): Try[Unit] = {
    boxesSumTry flatMap { openSum =>
      if (tx.newBoxes.map {
        case p: PolyBox => p.value
        case _ => 0L
      }.sum == openSum - tx.fee) {
        Success[Unit](Unit)
      } else {
        Failure(new Exception("Negative fee"))
      }
    }
  }

  def validateArbitTransfer(arT: ArbitTransfer): Try[Unit] = {

    val unlockers = generateUnlockers(arT.from, arT.signatures)

    val statefulValid: Try[Unit] = {
      val boxesSumTry: Try[Long] = {
        unlockers.foldLeft[Try[Long]](Success(0L))((partialRes, unlocker) =>

          partialRes.flatMap(partialSum =>
            /* Checks if unlocker is valid and if so adds to current running total */
            closedBox(unlocker.closedBoxId) match {
              case Some(box: ArbitBox) =>
                if (unlocker.boxKey.isValid(box
                  .proposition,
                  arT
                    .messageToSign)) {
                  Success(partialSum + box.value)
                } else {
                  Failure(new Exception(
                    "Incorrect unlocker"))
                }
              case None => Failure(new Exception(s"Box for unlocker $unlocker is not in the state"))
            }
          )

        )
      }
      //Determine enough arbits
      boxesSumTry flatMap { openSum =>
        if (arT.newBoxes.map {
          case p: ArbitBox => p.value
          case _ => 0L
        }.sum == openSum - arT.fee) {
          Success[Unit](Unit)
        } else {
          Failure(new Exception("Negative fee"))
        }
      }

    }

    statefulValid.flatMap(_ => semanticValidity(arT))
  }

  def validateAssetTransfer(asT: AssetTransfer): Try[Unit] = {

    val unlockers = generateUnlockers(asT.from, asT.signatures)

    val statefulValid: Try[Unit] = {
      val boxesSumTry: Try[Long] = {
        unlockers.foldLeft[Try[Long]](Success(0L))((partialRes, unlocker) =>
          partialRes.flatMap(partialSum =>
            /* Checks if unlocker is valid and if so adds to current running total */
            closedBox(unlocker.closedBoxId) match {
              case Some(box: AssetBox) =>
                if (unlocker.boxKey.isValid(box
                  .proposition,
                  asT
                    .messageToSign) && (box
                  .issuer equals asT.issuer) && (box
                  .assetCode equals asT.assetCode)) {
                  Success(partialSum + box.value)
                } else {
                  Failure(new Exception(
                    "Incorrect unlocker"))
                }
              case None => Failure(new Exception(s"Box for unlocker $unlocker is not in the state"))
            }
          )
        )
      }
      determineEnoughAssets(boxesSumTry, asT)
    }

    statefulValid.flatMap(_ => semanticValidity(asT))
  }

  private def determineEnoughAssets(boxesSumTry: Try[Long], tx: BifrostTransaction): Try[Unit] = {
    boxesSumTry flatMap { openSum =>
      if (tx.newBoxes.map {
        case a: AssetBox => a.value
        case _ => 0L
      }.sum <= openSum) {
        Success[Unit](Unit)
      } else {
        Failure(new Exception("Not enough assets"))
      }
    }
  }

  def validateProgramTransfer(prT: ProgramTransfer): Try[Unit] = {
    val from = Seq((prT.from, prT.executionBox.nonce))
    val signature = Map(prT.from -> prT.signature)
    val unlocker = generateUnlockers(from, signature).head

    val statefulValid: Try[Unit] = {
      prT.newBoxes.size match {
        case 1 => if(prT.newBoxes.head.isInstanceOf[ExecutionBox])
          Success[Unit](Unit)
        else
          Failure(new Exception("Incorrect box type"))

        case _ => Failure(new Exception("Incorrect number of boxes created"))
      }

      closedBox(unlocker.closedBoxId) match {
        case Some(box: ExecutionBox) => if(unlocker.boxKey.isValid(box.proposition, prT.messageToSign))
          Success[Unit](Unit)
        else
          Failure(new Exception("Incorrect unlocker"))

        case None => Failure(new Exception(s"Box for unlocker $unlocker is not in the state"))
      }
    }

    statefulValid.flatMap(_ => semanticValidity(prT))
  }

  def validateAssetCreation(ac: AssetCreation): Try[Unit] = {
    val statefulValid: Try[Unit] = {
      ac.newBoxes.size match {
          //only one box should be created
        case 1 => if (ac.newBoxes.head.isInstanceOf[AssetBox]) // the new box is an asset box
            Success[Unit](Unit)
          else
            Failure(new Exception("Incorrect box type"))

        case _ => Failure(new Exception("Incorrect number of boxes created"))
      }
    }
    statefulValid.flatMap(_ => semanticValidity(ac))
  }

  /**
    * Check the code is valid chain code and the newly created CodeBox is
    * formed properly
    *
    * @param cc : CodeCreation object
    * @return
    */
  def validateCodeCreation(cc: CodeCreation): Try[Unit] = {
    val statefulValid: Try[Unit] = {
      cc.newBoxes.size match {
        //only one box should be created
        case 1 => if (cc.newBoxes.head.isInstanceOf[CodeBox])
          Success[Unit](Unit)
        else
          Failure(new Exception("Incorrect box type"))

        case _ => Failure(new Exception("Incorrect number of boxes created"))
      }
    }
    statefulValid.flatMap(_ => semanticValidity(cc))
  }

  /**
    * Validates ProgramCreation instance on its unlockers && timestamp of the program
    *
    * @param pc : ProgramCreation object
    * @return
    */
  //noinspection ScalaStyle
  def validateProgramCreation(pc: ProgramCreation): Try[Unit] = {

    val unlockers = generateUnlockers(pc.boxIdsToOpen, pc.signatures.head._2)

    val unlockersValid: Try[Unit] = unlockers
      .foldLeft[Try[Unit]](Success())((unlockersValid, unlocker) =>
      unlockersValid
        .flatMap { unlockerValidity =>
          closedBox(unlocker.closedBoxId) match {
            case Some(box) =>
              if (unlocker.boxKey.isValid(
                box.proposition,
                pc.messageToSign)) {
                Success()
              } else {
                Failure(new TransactionValidationException("Incorrect unlocker"))
              }
            case None => Failure(new TransactionValidationException(s"Box for unlocker $unlocker is not in the state"))
          }
        }
    )

    val statefulValid = unlockersValid flatMap { _ =>

      val boxesAreNew = pc.newBoxes.forall(curBox => storage.get(ByteArrayWrapper(curBox.id)) match {
        case Some(_) => false
        case None => true
      })

      if (boxesAreNew) {
        Success[Unit](Unit)
      } else {
        Failure(new TransactionValidationException("ProgramCreation attempts to overwrite existing program"))
      }
    }

    statefulValid.flatMap(_ => semanticValidity(pc))
  }

  //noinspection ScalaStyle
  def validateProgramMethodExecution(pme: ProgramMethodExecution): Try[Unit] = {
    //TODO get execution box from box registry using UUID before using its actual id to get it from storage
    val executionBoxBytes = storage.get(ByteArrayWrapper(pme.executionBox.id))

    /* Program exists */
    if (executionBoxBytes.isEmpty) {
      throw new TransactionValidationException(s"Program ${Base58.encode(pme.executionBox.id)} does not exist")
    }

    val executionBox: ExecutionBox = ExecutionBoxSerializer.parseBytes(executionBoxBytes.get.data).get
    val programProposition: PublicKey25519Proposition =  executionBox.proposition

    /* This person belongs to program */
    if (!MultiSignature25519(pme.signatures.values.toSet).isValid(programProposition, pme.messageToSign)) {
      throw new TransactionValidationException(s"Signature is invalid for ExecutionBox")
    }

    val unlockers = generateUnlockers(pme.boxIdsToOpen, pme.signatures.head._2)

    val unlockersValid: Try[Unit] = unlockers
      .foldLeft[Try[Unit]](Success())((unlockersValid, unlocker) =>
      unlockersValid
        .flatMap { (unlockerValidity) =>
          closedBox(unlocker.closedBoxId) match {
            case Some(box) =>
              if (unlocker.boxKey.isValid(box.proposition, pme.messageToSign)) {
                Success()
              } else {
                Failure(new TransactionValidationException("Incorrect unlocker"))
              }
            case None => Failure(new TransactionValidationException(s"Box for unlocker $unlocker is not in the state"))
          }
        }
    )


    val statefulValid = unlockersValid flatMap { _ =>
      //Checks that newBoxes being created don't already exist
      val boxesAreNew = pme.newBoxes.forall(curBox => storage.get(ByteArrayWrapper(curBox.id)) match {
        case Some(_) => false
        case None => true
      })

      if (boxesAreNew) {
        Success[Unit](Unit)
      } else {
        Failure(new TransactionValidationException("ProgramCreation attempts to overwrite existing program"))
      }
    }


    statefulValid.flatMap(_ => semanticValidity(pme))
  }

  /**
    *
    * @param ar :  the AssetRedemption to validate
    * @return
    */
  //noinspection ScalaStyle
  def validateAssetRedemption(ar: AssetRedemption): Try[Unit] = {
    val unlockers = ar.boxIdsToOpen.map {
      boxId =>
        new BoxUnlocker[PublicKey25519Proposition] {
          override val closedBoxId: Array[Byte] = boxId
          override val boxKey: Signature25519 = ar.redemptionGroup(ByteArrayWrapper(boxId))
        }
    }

    val statefulValid: Try[Unit] = {
      /* First check that all the proposed boxes exist */
      val availableAssetsTry: Try[Map[String, Long]] = unlockers
        .foldLeft[Try[Map[String, Long]]](Success(Map[String, Long]()))((partialRes, unlocker) =>

        partialRes.flatMap(partialMap =>
          /* Checks if unlocker is valid and if so adds to current running total */
          closedBox(unlocker.closedBoxId) match {
            case Some(box: AssetBox) =>
              if (unlocker.boxKey.isValid(box.proposition, ar.messageToSign) && (box.issuer equals ar.issuer)) {
                Success(partialMap
                  .get(box.assetCode) match {
                  case Some(amount) => partialMap + (box.assetCode -> (amount + box.value))
                  case None => partialMap + (box.assetCode -> box.value)
                })
              } else {
                Failure(new TransactionValidationException("Incorrect unlocker"))
              }
            case None => Failure(
              new TransactionValidationException(s"Box for unlocker $unlocker is not in the state"))
          }
        )
      )

      /* Make sure that there's enough to cover the remainders */
      val enoughAssets = availableAssetsTry.flatMap(availableAssets =>
        ar.remainderAllocations
          .foldLeft[Try[Unit]](Success()) { case (partialRes, (assetCode, remainders)) =>
          partialRes.flatMap(_ => availableAssets.get(assetCode) match {
            case Some(amount) => if (amount > remainders.map(_._2).sum) {
              Success()
            } else {
              Failure(new TransactionValidationException("Not enough assets"))
            }
            case None => Failure(new TransactionValidationException("Asset not included in inputs"))
          })
        }
      )

      /* Handles fees */
      val boxesSumTry: Try[Long] = {
        unlockers
          .tail
          .foldLeft[Try[Long]](Success(0L))((partialRes, unlocker) => {
          partialRes
            .flatMap(total => closedBox(unlocker.closedBoxId) match {
              case Some(box: PolyBox) =>
                if (unlocker.boxKey.isValid(box.proposition, ar.messageToSign)) {
                  Success(total + box.value)
                } else {
                  Failure(new TransactionValidationException("Incorrect unlocker"))
                }
              case None => Failure(new TransactionValidationException(s"Box for unlocker $unlocker is not in the state"))
            })
        })
      }

      /* Incorrect unlocker or box provided, or not enough to cover declared fees */
      val enoughToCoverFees = Try {
        if (boxesSumTry.isFailure || boxesSumTry.get < ar.fee) {
          throw new TransactionValidationException("Insufficient balances provided for fees")
        }
      }
      enoughAssets.flatMap(_ => enoughToCoverFees)
    }
    statefulValid.flatMap(_ => semanticValidity(ar))
  }

  def validateCoinbaseTransaction(cb: CoinbaseTransaction): Try[Unit] = {
    val t = history.modifierById(cb.blockID).get
    def helper(m: BifrostBlock): Boolean = { m.id sameElements t.id }
    val validConstruction: Try[Unit] = {
      assert(cb.fee == 0L) // no fee for a coinbase tx
      //assert(cb.newBoxes.size == 1) // one one new box
      //assert(cb.newBoxes.head.isInstanceOf[ArbitBox]) // the new box is an arbit box
      // This will be implemented at a consensus level
      Try {
        // assert(cb.newBoxes.head.asInstanceOf[ArbitBox].value == history.modifierById(history.chainBack(history.bestBlock, helper).get.reverse(1)._2).get.inflation)
      }
    }
    validConstruction
  }

  def generateUnlockers(from: Seq[(PublicKey25519Proposition, Nonce)], signatures: Map[PublicKey25519Proposition, Signature25519]):
  Traversable[BoxUnlocker[PublicKey25519Proposition]] = {
    from.map {
      case (prop, nonce) =>
        new BoxUnlocker[PublicKey25519Proposition] {
          override val closedBoxId: Array[Byte] = PublicKeyNoncedBox.idFromBox(prop, nonce)
          override val boxKey: Signature25519 = signatures.getOrElse(prop, throw new Exception("Signature not provided"))
        }
    }
  }

  def generateUnlockers(boxIds: Seq[Array[Byte]], signature: Signature25519):
  Traversable[BoxUnlocker[PublicKey25519Proposition]] = {
    boxIds.map { id =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = id
        override val boxKey: Signature25519 = signature
      }
    }
  }
}

object BifrostState extends ScorexLogging {

  type T = Any
  type TX = BifrostTransaction
  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type BX = BifrostBox
  type BPMOD = BifrostBlock
  type GSC = GenericStateChanges[T, P, BX]
  type BSC = BifrostStateChanges

  //noinspection ScalaStyle
  def semanticValidity(tx: TX): Try[Unit] = {
    tx match {
      case poT: PolyTransfer => PolyTransfer.validate(poT)
      case arT: ArbitTransfer => ArbitTransfer.validate(arT)
      case asT: AssetTransfer => AssetTransfer.validate(asT)
      case prT: ProgramTransfer => ProgramTransfer.validate(prT)
      case ac: AssetCreation => AssetCreation.validate(ac)
      case cc: CodeCreation => CodeCreation.validate(cc)
      case pc: ProgramCreation => ProgramCreation.validate(pc)
      case cme: ProgramMethodExecution => ProgramMethodExecution.validate(cme)
      case ar: AssetRedemption => AssetRedemption.validate(ar)
      case cb: CoinbaseTransaction => CoinbaseTransaction.validate(cb)
      case _ => throw new UnsupportedOperationException(
        "Semantic validity not implemented for " + tx.getClass.toGenericString)
    }
  }

  //YT NOTE - byte array set quality is incorrectly overloaded (shallow not deep), consider using bytearraywrapper instead
  //YT NOTE - LSMStore will throw error if given duplicate keys in toRemove or toAppend so this needs to be fixed
  def changes(mod: BPMOD) : Try[GSC] = Try {
    val initial = (Set(): Set[Array[Byte]], Set(): Set[BX], 0L)

    val gen = mod.forgerBox.proposition

    val boxDeltas: Seq[(Set[Array[Byte]], Set[BX], Long)] = mod.transactions match {
      case Some(txSeq) => txSeq.map(tx => (tx.boxIdsToOpen.toSet, tx.newBoxes.toSet, tx.fee))
    }

    val (toRemove: Set[Array[Byte]], toAdd: Set[BX], reward: Long) =
      boxDeltas.foldLeft((Set[Array[Byte]](), Set[BX](), 0L))((aggregate, boxDelta) => {
        (aggregate._1 ++ boxDelta._1, aggregate._2 ++ boxDelta._2, aggregate._3 + boxDelta._3)
      })

    val rewardNonce = Longs.fromByteArray(mod.id.take(Longs.BYTES))

    var finalToAdd = toAdd
    if (reward != 0) finalToAdd += PolyBox(gen, rewardNonce, reward)

    //no reward additional to tx fees
    BifrostStateChanges(toRemove, finalToAdd, mod.timestamp)
  }


  def readOrGenerate(settings: ForgingSettings, callFromGenesis: Boolean = false, history: BifrostHistory): BifrostState = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
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
    val version = stateStorage
      .lastVersionID
      .fold(Array.emptyByteArray)(_.data)

    var timestamp: Long = 0L
    if (callFromGenesis) {
      timestamp = System.currentTimeMillis()
    } else {
      timestamp = Longs.fromByteArray(stateStorage
        .get(ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes)))
        .get
        .data)
    }

    val nodeKeys: Set[ByteArrayWrapper] = settings.nodeKeys.map(x => x.map(y => ByteArrayWrapper(Base58.decode(y).get))).orNull
    val pbr = ProgramBoxRegistry.readOrGenerate(settings, stateStorage).orNull
    val tbr = TokenBoxRegistry.readOrGenerate(settings, stateStorage).orNull
    if(pbr == null) log.info("Initializing state without programBoxRegistry") else log.info("Initializing state with programBoxRegistry")
    if(tbr == null) log.info("Initializing state without tokenBoxRegistry") else log.info("Initializing state with tokenBoxRegistry")
    if(nodeKeys != null) log.info(s"Initializing state to watch for public keys: ${nodeKeys.map(x => Base58.encode(x.data))}")
      else log.info("Initializing state to watch for all public keys")

    BifrostState(stateStorage, version, timestamp, history, pbr, tbr, nodeKeys)
  }

  def genesisState(settings: ForgingSettings, initialBlocks: Seq[BPMOD], history: BifrostHistory): BifrostState = {
    initialBlocks
      .foldLeft(readOrGenerate(settings, callFromGenesis = true, history)) {
        (state, mod) => state
          .changes(mod)
          .flatMap(cs => state.applyChanges(cs, mod.id))
          .get
    }
  }
}
