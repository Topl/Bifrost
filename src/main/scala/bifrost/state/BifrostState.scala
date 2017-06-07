package bifrost.state

import java.io.File
import java.time.Instant

import com.google.common.primitives.Longs
import bifrost.blocks.BifrostBlock
import bifrost.contract.Contract
import bifrost.scorexMod.{GenericBoxMinimalState, GenericStateChanges}
import bifrost.transaction._
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.MofNProposition
import bifrost.transaction.proof.MultiSignature25519
import io.circe.{Json, JsonObject}
import io.circe.syntax._
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.{PrivateKey25519, StateChanges}
import scorex.core.utils.ScorexLogging
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
  * @param storage: singleton Iodb storage instance
  * @param version: blockId used to identify each block. Also used for rollback
  * @param timestamp: timestamp of the block that results in this state
  */
case class BifrostState(storage: LSMStore, override val version: VersionTag, timestamp: Long)
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

  private def getProfileBox(prop: PublicKey25519Proposition, field: String): Try[ProfileBox] = {
    val boxBytes = storage.get(ByteArrayWrapper(ProfileBox.idFromBox(prop, field)))
    ProfileBoxSerializer.parseBytes(boxBytes.get.data)
  }

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
      val timestamp: Long = Longs.fromByteArray(storage.get(ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes))).get.data)
      BifrostState(storage, version, timestamp)
    }
  }

  override def changes(mod: BPMOD): Try[GSC] = BifrostState.changes(mod)

  override def applyChanges(changes: GSC, newVersion: VersionTag): Try[NVCT] = Try {

    val boxIdsToRemove = changes.boxIdsToRemove.map(ByteArrayWrapper.apply)

    // TODO check if b.bytes screws up compared to BifrostBoxCompanion.toBytes
    val boxesToAdd = changes.toAppend.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    log.debug(s"Update BifrostState from version $lastVersionString to version ${Base58.encode(newVersion)}. " +
      s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
      s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}")

    val timestamp: Long = changes.asInstanceOf[BifrostStateChanges].timestamp

    if (storage.lastVersionID.isDefined) boxIdsToRemove.foreach(i => require(closedBox(i.data).isDefined))

    storage.update(
      ByteArrayWrapper(newVersion),
      boxIdsToRemove,
      boxesToAdd + (ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes)) -> ByteArrayWrapper(Longs.toByteArray(timestamp)))
    )

    val newSt = BifrostState(storage, newVersion, timestamp)
    boxIdsToRemove.foreach(box => require(newSt.closedBox(box.data).isEmpty, s"Box $box is still in state"))
    newSt

  }

  override def validate(transaction: TX): Try[Unit] = transaction match {
    case poT: PolyTransfer => validatePolyTransfer(poT)
    case cc: ContractCreation => validateContractCreation(cc)
    case prT: ProfileTransaction => validateProfileTransaction(prT)
    case cme: ContractMethodExecution => validateContractMethodExecution(cme)
    case cComp: ContractCompletion => validateContractCompletion(cComp)
  }

  /**
    *
    * @param poT: the PolyTransfer to validate
    * @return
    */
  def validatePolyTransfer(poT: PolyTransfer): Try[Unit] = Try {

    val statefulValid: Try[Unit] = {

      val boxesSumTry: Try[Long] = {
        poT.unlockers.foldLeft[Try[Long]](Success(0L))((partialRes, unlocker) =>

          partialRes.flatMap(partialSum =>
            /* Checks if unlocker is valid and if so adds to current running total */
            closedBox(unlocker.closedBoxId) match {
              case Some(box: PolyBox) =>
                if (unlocker.boxKey.isValid(box.proposition, poT.messageToSign)) {
                  Success(partialSum + box.value)
                } else {
                  Failure(new Exception("Incorrect unlocker"))
                }
              case None => Failure(new Exception(s"Box for unlocker $unlocker is not in the state"))
            }
          )

        )
      }

      boxesSumTry flatMap { openSum =>
        if (poT.newBoxes.map {
          case p: PolyBox => p.value
          case _ => 0L
        }.sum == openSum - poT.fee) {
          Success[Unit](Unit)
        } else {
          Failure(new Exception("Negative fee"))
        }
      }

    }

    statefulValid.flatMap(_ => semanticValidity(poT))
  }

  /**
    *
    * @param pt: ProfileTransaction
    * @return success or failure
    */
  def validateProfileTransaction(pt: ProfileTransaction): Try[Unit] = Try {
    /* Make sure there are no existing boxes of the all fields in tx
    *  If there is one box that exists then the tx is invalid
    * */
    val boxesExist: Boolean = pt.newBoxes.forall(curBox => {
      val pBox = curBox.asInstanceOf[ProfileBox]
      val boxBytes = storage.get(ByteArrayWrapper(ProfileBox.idFromBox(pBox.proposition, pBox.key)))
      boxBytes match {
        case None => false
        case _ => ProfileBoxSerializer.parseBytes(boxBytes.get.data).isSuccess
      }
    })
    require(!boxesExist)

    semanticValidity(pt)
  }

  /**
    * validates ContractCreation instance on its unlockers && timestamp of the contract
    *
    * @param cc: ContractCreation object
    * @return
    */
  //noinspection ScalaStyle
  def validateContractCreation(cc: ContractCreation): Try[Unit] = {

    /* First check to see all roles are present */
    val roleBoxAttempts: Map[PublicKey25519Proposition, Try[ProfileBox]] = cc.signatures.filter { case (prop, sig) =>
      // Verify that this is being sent by this party because we rely on that during ContractMethodExecution
      sig.isValid(prop, cc.messageToSign)
    }.map { case (prop, _) => (prop, getProfileBox(prop, "role")) }


    val roleBoxes: Iterable[String] = roleBoxAttempts collect { case s: (PublicKey25519Proposition, Try[ProfileBox]) if s._2.isSuccess => s._2.get.value }

    if (!Set(Role.Producer.toString, Role.Hub.toString, Role.Investor.toString).equals(roleBoxes.toSet)) {
      log.debug("Not all roles were fulfilled for this transaction")
      return Failure(new Exception("Not all roles were fulfilled for this transaction"))
    } else if (roleBoxes.size > 3) {
      log.debug("Too many signatures for the parties of this transaction")
      return Failure(new Exception("Too many signatures for the parties of this transaction"))
    }

    /* Verifies that the role boxes match the roles stated in the contract creation */
    if (!roleBoxes.zip(cc.parties.keys).forall { case (boxRole, role) => boxRole.equals(role.toString) }) {
      log.debug("role boxes does not match the roles stated in the contract creation")
      return Failure(new Exception("role boxes does not match the roles stated in the contract creation"))
    }

    val unlockersValid: Try[Unit] = cc.unlockers.foldLeft[Try[Unit]](Success())((unlockersValid, unlocker) =>

      unlockersValid.flatMap { (unlockerValidity) =>
        closedBox(unlocker.closedBoxId) match {
          case Some(box) =>
            if (unlocker.boxKey.isValid(box.proposition, cc.messageToSign)) {
              Success()
            } else {
              Failure(new Exception("Incorrect unlocker"))
            }
          case None => Failure(new Exception(s"Box for unlocker $unlocker is not in the state"))
        }
      }
    )

    val statefulValid = unlockersValid flatMap { _ =>

      val boxesAreNew = cc.newBoxes.forall(curBox => storage.get(ByteArrayWrapper(curBox.asInstanceOf[ContractBox].id)) match {
        case Some(box) => false
        case None => true
      })

      val txTimestampIsAcceptable = cc.timestamp > timestamp && timestamp < Instant.now().toEpochMilli


      if (boxesAreNew && txTimestampIsAcceptable) {
        Success[Unit](Unit)
      } else {
        Failure(new Exception("Boxes attempt to overwrite existing contract"))
      }
    }

    statefulValid.flatMap(_ => semanticValidity(cc))

    // TODO check whether hub has sufficient room
  }

  /**
    *
    * @param cme: the ContractMethodExecution to validate
    * @return
    */
  def validateContractMethodExecution(cme: ContractMethodExecution): Try[Unit] = {

    val contractBytes = storage.get(ByteArrayWrapper(cme.contractBox.id))

    //TODO fee verification

    /* Contract exists */
    if(contractBytes.isEmpty) {
      Failure(new NoSuchElementException(s"Contract ${cme.contractBox.id} does not exist"))

    } else {

      val contractBox: ContractBox = ContractBoxSerializer.parseBytes(contractBytes.get.data).get
      val contractProposition: MofNProposition = contractBox.proposition
      val contract: Contract = Contract((contractBox.json \\ "value").head, contractBox.id)
      val effectiveDate: Long = contract.agreement("contractEffectiveTime").get.asNumber.get.toLong.get
      val profileBox = getProfileBox(cme.parties.values.head, "role").get

      /* This person belongs to contract */
      if (!MultiSignature25519(cme.signatures.values.toSet).isValid(contractProposition, cme.messageToSign)) {
        Failure(new IllegalAccessException(s"Signature is invalid for contractBox"))

      /* Signature matches profilebox owner */
      } else if (!cme.signatures.values.head.isValid(profileBox.proposition, cme.messageToSign)) {
        Failure(new IllegalAccessException(s"Signature is invalid for ${Base58.encode(cme.parties.values.head.pubKeyBytes)} profileBox"))

      /* Role provided by CME matches profilebox */
      } else if (!profileBox.value.equals(cme.parties.keys.head.toString)) {
        Failure(new IllegalAccessException(s"Role ${cme.parties.keys.head} for ${Base58.encode(cme.parties.values.head.pubKeyBytes)} does not match ${profileBox.value} in profileBox"))

      /* Timestamp is after most recent block, not in future */
      } else if (cme.timestamp <= timestamp || cme.timestamp > Instant.now.toEpochMilli) {
        Failure(new Exception("Unacceptable timestamp"))

      } else if (effectiveDate > Instant.now.toEpochMilli) {
        Failure(new Exception("Effective date hasn't passed"))

      } else {
        semanticValidity(cme)
      }

    }
  }

  /**
    *
    * @param cc: the ContractMethodExecution to validate
    * @return
    */
  //noinspection ScalaStyle
  def validateContractCompletion(cc: ContractCompletion): Try[Unit] = {

    val contractBytes = storage.get(ByteArrayWrapper(cc.contractBox.id))
    //TODO fee verification

    /* Contract exists */
    if (contractBytes.isEmpty)
      Failure(new NoSuchElementException(s"Contract ${cc.contractBox.id} does not exist"))

    else {
      val contractJson: Json = ContractBoxSerializer.parseBytes(contractBytes.get.data).get.json
      val contractProposition: MofNProposition = ContractBoxSerializer.parseBytes(contractBytes.get.data).get.proposition

      /* Checking that all of the parties are part of the contract and have claimed roles */
      val verifyParties = Try {
        require(cc.parties.map {
          case (role, proposition) =>
            val profileBox = getProfileBox(proposition, "role").get

            /* This person belongs to contract */
            if (!MultiSignature25519(Set(cc.signatures(proposition))).isValid(contractProposition, cc.messageToSign))
              throw new IllegalAccessException(s"Signature is invalid for contractBox")

            /* Signature matches profilebox owner */
            else if (!cc.signatures(proposition).isValid(profileBox.proposition, cc.messageToSign))
              throw new IllegalAccessException(s"Signature is invalid for ${Base58.encode(proposition.pubKeyBytes)} profileBox")

            /* Role provided by cc matches profilebox */
            else if (!profileBox.value.equals(role.toString))
              throw new IllegalAccessException(s"Role $role for ${Base58.encode(proposition.pubKeyBytes)} does not match ${profileBox.value} in profileBox")

            role

          /* Checking that all the roles are represented */
        }.toSet.equals(Set(Role.Investor, Role.Hub, Role.Producer)))
      }

      if (verifyParties.isFailure) verifyParties

      /* Make sure timestamp is after most recent block, not in future */
      else if (cc.timestamp <= timestamp || timestamp >= Instant.now().toEpochMilli)
        Failure(new Exception("Unacceptable timestamp"))

      /* Contract is in completed state, waiting for completion */
      else {
        val endorsementsJsonObj: JsonObject = cc.contract.storage("endorsements").getOrElse(Map[String, Json]().asJson).asObject.get
        val endorseAttempt = endorsementsJsonObj(Base58.encode(cc.parties.head._2.pubKeyBytes))

        val allEndorsedAndAgree = Try {
          cc.parties.tail.foldLeft(endorseAttempt.get.asString.get)((a, b) => {
            require(endorsementsJsonObj(Base58.encode(b._2.pubKeyBytes)).get.asString.get == a)
            a
          })
        }

        val producerProposition = cc.parties.find(_._1 == Role.Producer).map(_._2).get
        val producerReputationIsValid = Try {
          cc.producerReputation.foreach(claimedBox => {
            val box: ReputationBox = closedBox(claimedBox.id).get.asInstanceOf[ReputationBox]

            if (box.proposition != producerProposition)
              throw new Exception(s"Claimed reputation box had proposition $producerProposition but actually had ${box.proposition}")

            if (box.value != claimedBox.value)
              throw new Exception(s"Claimed reputation box with value ${claimedBox.value} but actually had ${box.value}")

          })
        }

        allEndorsedAndAgree.flatMap(_ => producerReputationIsValid).flatMap(_ => semanticValidity(cc))
      }
    }
  }
}

object BifrostState {

  type T = Any
  type TX = BifrostTransaction
  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type BX = BifrostBox
  type BPMOD = BifrostBlock
  type GSC = GenericStateChanges[T, P, BX]
  type BSC = BifrostStateChanges

  def semanticValidity(tx: TX): Try[Unit] = {
    tx match {
      case poT: PolyTransfer => PolyTransfer.validate(poT)
      case arT: ArbitTransfer => ArbitTransfer.validate(arT)
      case cc: ContractCreation => ContractCreation.validate(cc)
      case ccomp: ContractCompletion => ContractCompletion.validate(ccomp)
      case prT: ProfileTransaction => ProfileTransaction.validate(prT)
      case cme: ContractMethodExecution => ContractMethodExecution.validate(cme)
      case _ => Failure(new Exception("Semantic validity not implemented for " + tx.getClass.toGenericString))
    }
  }


  def changes(mod: BPMOD): Try[GSC] = {
    Try {
      val initial = (Set(): Set[Array[Byte]], Set(): Set[BX], 0L)

      val boxDeltas: Seq[(Set[Array[Byte]], Set[BX], Long)] = mod.transactions match {
        case Some(txSeq) => txSeq.map {
          // (rm, add, fee)
          case sc: PolyTransfer => (sc.boxIdsToOpen.toSet, sc.newBoxes.toSet, sc.fee)
          case at: ArbitTransfer => (at.boxIdsToOpen.toSet, at.newBoxes.toSet, at.fee)
          case cc: ContractCreation => (cc.boxIdsToOpen.toSet, cc.newBoxes.toSet, cc.fee)
          case cme: ContractMethodExecution => (cme.boxIdsToOpen.toSet, cme.newBoxes.toSet, cme.fee)
          case ccomp: ContractCompletion => (ccomp.boxIdsToOpen.toSet, ccomp.newBoxes.toSet, ccomp.fee)
          case pt: ProfileTransaction => (pt.boxIdsToOpen.toSet, pt.newBoxes.toSet, pt.fee)
        }
      }

      val (toRemove: Set[Array[Byte]], toAdd: Set[BX], reward: Long) =
        boxDeltas.foldLeft((Set[Array[Byte]](), Set[BX](), 0L))((aggregate, boxDelta) => {
          (aggregate._1 ++ boxDelta._1, aggregate._2 ++ boxDelta._2, aggregate._3 + boxDelta._3 )
        })

      //no reward additional to tx fees
      BifrostStateChanges(toRemove, toAdd, mod.timestamp)
    }
  }

  def readOrGenerate(settings: Settings, callFromGenesis: Boolean = false): BifrostState = {
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
    val version = stateStorage.lastVersionID.fold(Array.emptyByteArray)(_.data)

    var timestamp: Long = 0L
    if (callFromGenesis) {
      timestamp = System.currentTimeMillis()
    } else {
      timestamp = Longs.fromByteArray(stateStorage.get(ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes))).get.data)
    }

    BifrostState(stateStorage, version, timestamp)
  }

  def genesisState(settings: Settings, initialBlocks: Seq[BPMOD]): BifrostState = {
    initialBlocks.foldLeft(readOrGenerate(settings, callFromGenesis = true)) { (state, mod) =>
      state.changes(mod).flatMap(cs => state.applyChanges(cs, mod.id)).get
    }
  }
}