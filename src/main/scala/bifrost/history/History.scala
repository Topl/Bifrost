package bifrost.history

import java.io.File

import bifrost.consensus.{DifficultyBlockValidator, ModifierSemanticValidity}
import bifrost.history.GenericHistory._
import bifrost.history.History.GenesisParentId
import bifrost.modifier.ModifierId
import bifrost.modifier.block.{Block, BlockValidator, Bloom}
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.network.message.BifrostSyncInfo
import bifrost.nodeView.NodeViewModifier
import bifrost.nodeView.NodeViewModifier.{ModifierTypeId, bytesToId, idToBytes}
import bifrost.settings.AppSettings
import bifrost.utils.{BifrostEncoding, Logging}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

import scala.annotation.tailrec
import scala.collection.BitSet
import scala.concurrent.duration.MILLISECONDS
import scala.math.{max, min}
import scala.util.{Failure, Success, Try}

/**
  * A representation of the entire blockchain (whether it's a blocktree, blockchain, etc.)
  *
  * @param storage    a wrapper primarily for the LSMStore and for storage of the minimal state
  * @param settings   settings regarding updating forging difficulty, constants, etc.
  * @param validators rule sets that dictate validity of blocks in the history
  */
class History(val storage: Storage, settings: AppSettings, validators: Seq[BlockValidator[Block]])
  extends GenericHistory[Block, BifrostSyncInfo, History]
    with Logging with BifrostEncoding {

  override type NVCT = History

  require(NodeViewModifier.ModifierIdSize == 32, "32 bytes ids assumed")

  lazy val height: Long = storage.chainHeight
  lazy val score: BigInt = storage.bestChainScore
  lazy val bestBlockId: ModifierId = storage.bestBlockId
  lazy val difficulty: Long = storage.difficultyOf(bestBlockId).get
  lazy val bestBlock: Block = storage.bestBlock


  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = height <= 0

  override def applicable(block: Block): Boolean = {
    contains(block.parentId)
  }

  override def modifierById(id: ModifierId): Option[Block] = storage.modifierById(id)

  override def contains(id: ModifierId): Boolean =
    if (id.hashBytes sameElements History.GenesisParentId) true else modifierById(id).isDefined

  /**
    * Adds block to chain and updates storage (difficulty, score, etc.) relating to that
    *
    * @param block block to append
    * @return the update history including `block` as the most recent block
    */
  override def append(block: Block): Try[(History, ProgressInfo[Block])] = Try {

    log.debug(s"Trying to append block ${block.id} to history")

    val validationResults = validators.map(_.validate(block))

    // TODO: JAA - shouldn't we reject blocks that fail validation?
    validationResults.foreach {
      case Failure(e) => log.warn(s"Block validation failed", e)
      case _ =>
    }
    validationResults.foreach(_.get)


    val res: (History, ProgressInfo[Block]) = {

      if (isGenesis(block)) {
        storage.update(block, settings.forgingSettings.InitialDifficulty, isBest = true)
        val progInfo = ProgressInfo(None, Seq.empty, Seq(block), Seq.empty)
        (new History(storage, settings, validators), progInfo)
      } else {

        // Calculate the block difficulty according to
        // https://nxtdocs.jelurida.com/Nxt_Whitepaper#Block_Creation_.28Forging.29
        val blockTimes = lastBlocks(4, block).map(prev => prev.timestamp)
        val averageDelay = (blockTimes drop 1, blockTimes).zipped.map(_-_).sum / (blockTimes.length - 1)
        val parentDifficulty = storage.difficultyOf(block.parentId).get
        val targetTime = settings.forgingSettings.targetBlockTime.toUnit(MILLISECONDS)
        // magic numbers here (1.1, 0.9, and 0.64) are straight from NXT
        val difficulty: Long = if (averageDelay > targetTime) {
          (parentDifficulty * min(averageDelay, targetTime * 1.1) / targetTime).toLong
        } else {
          (parentDifficulty * (1 - 0.64 * (1 - (max(averageDelay, targetTime * 0.9) / targetTime) ))).toLong
        }

        // Determine if this block is on the canonical chain
        val builtOnBestChain = applicable(block)
        // Check that the new block's parent is the last best block
        val mod: ProgressInfo[Block] = if (!builtOnBestChain) {
          log.debug(s"New orphaned block ${block.id.toString}")
          ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
        } else if (block.parentId.hashBytes sameElements storage.bestBlockId.hashBytes) { // new block parent is best block so far
          log.debug(s"New best block ${block.id.toString}")
          ProgressInfo(None, Seq.empty, Seq(block), Seq.empty)
        } else { // we want to swap to a fork
          bestForkChanges(block)
        }

        storage.update(block, difficulty, builtOnBestChain)
        (new History(storage, settings, validators), mod)
      }
    }
    log.info(s"History: block ${block.id} appended to chain with score ${storage.scoreOf(block.id)}. " +
               s"Best score is $score. Pair: $bestBlockId")
    res
  }

  /**
    * Removes this block (and its children) from the history and rolls back to the state after the parent block
    *
    * Maybe we should check drop the id passed in is the best block's id? or just get rid of the arg
    *
    * @param modifierId the id of the block to chop off
    * @return a new history which is the current history ending at the parent of the dropped block
    */
  override def drop(modifierId: ModifierId): History = {

    val block = storage.modifierById(modifierId).get
    val parentBlock = storage.modifierById(block.parentId).get

    log.debug(s"Failed to apply block. Rollback BifrostState to ${parentBlock.id} from version ${block.id}")
    storage.rollback(parentBlock.id)
    new History(storage, settings, validators)
  }

  /**
    * Calculates the changes necessary for a swap to a fork
    *
    * @param block the block that has a parent on the canonical chain but is not on the chain
    * @return ProgressInfo that specifies the blocks to delete (worse fork) and blocks to add (better fork)
    */
  def bestForkChanges(block: Block): ProgressInfo[Block] = {

    /* Get the two branches of the fork including their common block */
    val (newSuffix, oldSuffix) = commonBlockThenSuffixes(modifierById(block.parentId).get)

    log.debug(s"Processing fork for block ${block.id}: \n" +
                s"old: $oldSuffix\n" +
                s"new: $newSuffix")

    /* Roll back to the common block */
    val rollbackPoint = newSuffix.headOption
    val throwBlocks = oldSuffix.tail.map(id => modifierById(id).get)

    /* Add the newSuffix to the rolled-back chain */
    val applyBlocks = newSuffix.tail.map(id => modifierById(id).get) ++ Seq(block)

    require(applyBlocks.nonEmpty)
    require(throwBlocks.nonEmpty)

    ProgressInfo[Block](rollbackPoint, throwBlocks, applyBlocks, Seq.empty)
  }

  /**
    * Forms a list of available blocks to build upon
    *
    * @return the blocks that are "exposed" for use
    */
  override def openSurfaceIds(): Seq[ModifierId] =
    if (isEmpty) {
      Seq(bytesToId(History.GenesisParentId))
    } else {
      Seq(bestBlockId)
    } // TODO return sequence of exposed endpoints?


  //TODO used in tests, but should replace with HistoryReader.continuationIds
  /**
    * Gather blocks from after `from` that should be added to the chain
    *
    * @param from the list of known blocks from which to gather continuation
    * @param size the number of blocks to return after `from`
    * @return
    */
  def continuationIds(from: Seq[(ModifierTypeId, ModifierId)],
                               size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = {

    /* Whether m is a genesis block or is in `from` */
    def inList(m: Block): Boolean = idInList(m.id) || isGenesis(m)

    def idInList(id: ModifierId): Boolean = from.exists(f => f._2 == id)

    /* Extend chain back until end of `from` is found, then return <size> blocks continuing from that point */
    chainBack(bestBlock, inList) match {
      case Some(chain) if chain.exists(id => idInList(id._2)) => Some(chain.take(size))
      case Some(_) =>
        log.warn("Found chain without ids from remote")
        None
      case _ => None
    }
  }

  /**
    * Return specified number of Bifrost blocks, ordered back from last one
    *
    * @param count - how many blocks to return
    * @return blocks, in reverse order (starting from the most recent one)
    */
  def lastBlocks(count: Int, startBlock: Block): Seq[Block] = {
    @tailrec
    def loop(b: Block, acc: Seq[Block] = Seq()): Seq[Block] = {
      if (acc.length >= count) acc
      else modifierById(b.parentId) match {
          case Some(parent: Block) => loop(parent, b +: acc)
          case _ => b +: acc
        }
    }

    if (isEmpty) Seq()
    else loop(startBlock)
  }

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param info other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(info: BifrostSyncInfo): HistoryComparisonResult = {
    Option(bestBlockId) match {
      case Some(id) if info.lastBlockIds.lastOption.contains(id) =>
        //Our best header is the same as other node best header
        Equal
      case Some(id) if info.lastBlockIds.contains(id) =>
        //Our best header is in other node best chain, but not at the last position
        Older
      case Some(_) if info.lastBlockIds.isEmpty =>
        //Other history is empty, our contain some headers
        Younger
      case Some(_) =>
        //We are on different forks now.
        if (info.lastBlockIds.view.reverse.exists(m => contains(m))) {
          //Return Younger, because we can send blocks from our fork that other node can download.
          Fork
        } else {
          //We don't have any of id's from other's node sync info in history.
          //We don't know whether we can sync with it and what blocks to send in Inv message.
          //Assume it is older and far ahead from us
          Older
        }
      case None if info.lastBlockIds.isEmpty =>
        //Both nodes do not keep any blocks
        Equal
      case None =>
        //Our history is empty, other contain some headers
        Older
    }
  }

  private def isGenesis(b: Block): Boolean = storage.isGenesis(b)

  def blockForger(m: Block): PublicKey25519Proposition = m.forgerBox.proposition

  /**
    * Calculates the distribution of blocks to forgers
    *
    * @return a map from public keys of forgers to the number of blocks they have forged
    */
  def forgerDistribution(): Map[PublicKey25519Proposition, Int] = {
    val map = collection.mutable.Map[PublicKey25519Proposition, Int]().withDefaultValue(0)

    /**
      * Finds the forger for this block, increments their block number entry in `map`, and continues down the chain
      *
      * @param m the current block for which to increment the forger entry
      */
    @tailrec
    def loopBackAndIncrementForger(m: Block): Unit = {
      val forger = blockForger(m)
      map.update(forger, map(forger) + 1)
      parentBlock(m) match {
        case Some(parent) => loopBackAndIncrementForger(parent)
        case None =>
      }
    }

    loopBackAndIncrementForger(bestBlock)
    map.toMap
  }

  def count(f: Block => Boolean): Int = filter(f).length

  def filter(f: Block => Boolean): Seq[Block] = {
    @tailrec
    def loop(m: Block, acc: Seq[Block]): Seq[Block] = parentBlock(m) match {
      case Some(parent) => if (f(m)) loop(parent, m +: acc) else loop(parent, acc)
      case None => if (f(m)) m +: acc else acc
    }

    loop(bestBlock, Seq())
  }

  /**
    *
    * @param f : predicate that tests whether a queryBloom is compatible with a block's bloom
    * @return Seq of blockId that satisfies f
    */
  def getBlockIdsByBloom(f: BitSet => Boolean): Seq[ModifierId] = {
    @tailrec
    def loop(current: Array[Byte], acc: Seq[Array[Byte]]): Seq[ModifierId] = storage.serializedParentIdOf(current) match {
      case Some(value) =>
        if (f(storage.bloomOf(current).get)) loop(value, current +: acc) else loop(value, acc)
      case None =>
        if (f(storage.bloomOf(current).get)) (current +: acc).map(bytesToId(_)) else acc.map(bytesToId(_))
    }

    loop(idToBytes(storage.bestBlockId), Seq())
  }

  def bloomFilter(queryBloomTopics: IndexedSeq[Array[Byte]]): Seq[Transaction] = {
    val queryBloom: BitSet = Bloom.calcBloom(queryBloomTopics.head, queryBloomTopics.tail)
    val f: BitSet => Boolean = {
      blockBloom =>
        val andRes = blockBloom & queryBloom
        queryBloom equals andRes
    }
    // Go through all pertinent txs to filter out false positives
    getBlockIdsByBloom(f).flatMap(b =>
      modifierById(b).get.txs.filter(tx =>
        tx.bloomTopics match {
          case Some(txBlooms) =>
            var res = false
            val txBloomsWrapper = txBlooms.map(ByteArrayWrapper(_))
            val queryBloomsWrapper = queryBloomTopics.map(ByteArrayWrapper(_))
            res = txBloomsWrapper.intersect(queryBloomsWrapper).length == queryBloomsWrapper.length
            res
          case None => false
        }
    ))
  }

  def parentBlock(m: Block): Option[Block] = modifierById(m.parentId)

  /**
    * Go back through chain and get block ids until condition `until` is satisfied
    *
    * @param m     the modifier to start at
    * @param until the condition that indicates (when true) that recursion should stop
    * @param limit the maximum number of blocks to recurse back
    * @return the sequence of block information (TypeId, Id) that were collected until `until` was satisfied
    *         (None only if the parent for a block was not found) starting from the original `m`
    */
  final def chainBack(m: Block,
                      until: Block => Boolean,
                      limit: Int = Int.MaxValue): Option[Seq[(ModifierTypeId, ModifierId)]] = {

    @tailrec
    def loop(block: Block, acc: Seq[Block]): Seq[Block] = {
      if(acc.lengthCompare(limit) == 0 || until(block)) {
        acc
      } else {
        parentBlock(block) match {
          case Some(parent: Block) ⇒ loop(parent, acc :+ parent)
          case None if acc.contains(block) ⇒ acc
          case _ ⇒ acc :+ block
        }
      }
    }

    if(limit == 0) {
      None
    } else {
      Option(loop(m, Seq(m)).map(b ⇒ (b.modifierTypeId, b.id)).reverse)
    }
  }

  /**
    * Find common suffixes for two chains starting from forkBlock
    *
    * @param forkBlock the common source of a potential fork (normally just the parent of bestBlock)
    * @param limit     how far back to check for a common block
    * @return sequences which contain the last common block and variant blocks for the chains
    */
  final def commonBlockThenSuffixes(forkBlock: Block,
                                    limit: Int = Int.MaxValue): (Seq[ModifierId], Seq[ModifierId]) = {

    /* The entire chain that was "best" */
    val loserChain = chainBack(bestBlock, isGenesis, limit).get.map(_._2)

    /* `in` specifies whether `loserChain` has this block */
    def in(m: Block): Boolean = loserChain.contains(m.id)

    /* Finds the chain of blocks back from `forkBlock` until a common block to `loserChain` is found */
    val winnerChain = chainBack(forkBlock, in, limit).get.map(_._2)

    val i = loserChain.indexWhere(id => id == winnerChain.head)

    /* The two segments including their common block */
    (winnerChain, loserChain.takeRight(loserChain.length - i))

  }.ensuring(r => r._1.head == r._2.head)

  /**
    * Average delay in milliseconds between last $blockNum blocks starting from $block
    * Debug only
    */
  def averageDelay(id: ModifierId, blockNum: Int): Try[Long] = Try {
    val block = modifierById(id).get
    val c = chainBack(block, isGenesis, blockNum).get.map(_._2)
    (block.timestamp - modifierById(c.head).get.timestamp) / c.length
  }

  //chain without brothers
  override def toString: String = {
    chainBack(bestBlock, isGenesis).get.map(_._2).mkString(",")
  }

  /**
    * Report that modifier is valid from point of view of the state component
    *
    * @param modifier - valid modifier
    * @return modified history
    */
  override def reportModifierIsValid(modifier: Block): History = {
    log.debug(s"Modifier ${modifier.id} is a valid block")
    this
  }

  /**
    * Report that modifier is invalid from other nodeViewHolder components point of view
    *
    * @param modifier     - invalid modifier
    * @param progressInfo - what suffix failed to be applied because of an invalid modifier
    * @return modified history and new progress info
    */
  override def reportModifierIsInvalid(modifier: Block, progressInfo: ProgressInfo[Block]): (History, ProgressInfo[Block]) = ???

  /**
    * Whether a modifier could be applied to the history
    *
    * @param modifier - modifier to apply
    * @return `Success` if modifier can be applied, `Failure(ModifierError)` if can not
    */
  override def applicableTry(modifier: Block): Try[Unit] = {
    modifier match {
      case b: Block ⇒ Success(())
    }
  }

  /**
    * Return semantic validity status of modifier with id == modifierId
    *
    * @param modifierId - modifier id to check
    * @return
    */
  override def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity = ???

  /**
    * Ids of modifiers, that node with info should download and apply to synchronize
    */
  override def continuationIds(info: BifrostSyncInfo, size: Int): ModifierIds = {
    if(isEmpty) {
      info.startingPoints
    } else if(info.lastBlockIds.isEmpty) {
      val heightFrom = Math.min(height, size)
      val block = storage.modifierById(storage.idAtHeight(heightFrom)).get
      chainBack(block, _ ⇒ false, size).get
    } else {
      val ids = info.lastBlockIds
      val branchPointOpt: Option[ModifierId] = ids.view.reverse
        .find(m ⇒ storage.modifierById(m).isDefined).orElse(None)
      branchPointOpt.toSeq.flatMap { branchPoint ⇒
        val remoteHeight = storage.heightOf(branchPoint).get
        val heightFrom = Math.min(height, remoteHeight + size)
        val startBlock = storage.modifierById(storage.idAtHeight(heightFrom)).get
        chainBack(startBlock, _.parentId == branchPoint, size).get
      }
    }
  }

  /**
    * Information about our node synchronization status. Other node should be able to compare it's view with ours by
    * this syncInfo message and calculate modifiers missed by our node.
    *
    * @return
    */
  override def syncInfo: BifrostSyncInfo =
    if(isEmpty) {
      BifrostSyncInfo(Seq.empty)

    } else {
      val startingPoints = lastHeaders(BifrostSyncInfo.MaxLastBlocks)

      if(startingPoints.headOption.exists(x ⇒ isGenesis(modifierById(x).get))) {
        BifrostSyncInfo(ModifierId(GenesisParentId) +: startingPoints)

      } else {
        BifrostSyncInfo(startingPoints)
      }
    }

  /**
    * Return last count headers from best headers chain if exist or chain up to genesis otherwise
    */
  def lastHeaders(count: Int, offset: Int = 0): IndexedSeq[ModifierId] =
    lastBlocks(count, bestBlock).map(block => block.id).toIndexedSeq

//  /**
//    * @param height - block height
//    * @return ids of headers on chosen height.
//    *         Seq.empty we don't have any headers on this height (e.g. it is too big or we bootstrap in PoPoW regime)
//    *         single id if no forks on this height
//    *         multiple ids if there are forks at chosen height.
//    *         First id is always from the best headers chain.
//    */
  /*def idsAtHeight(height: Int): Seq[ModifierId] =
    storage.getIndex(heightIdsKey(height: Int))
      .getOrElse(Array()).grouped(32).map(bytesToId).toSeq
   */
}


object History extends Logging {

  val GenesisParentId: Array[Byte] = Array.fill(32)(1: Byte)

  def readOrGenerate(settings: AppSettings): History = {
    val dataDirOpt = settings.dataDir.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get
    readOrGenerate(dataDir, settings)
  }

  def readOrGenerate(dataDir: String, settings: AppSettings): History = {
    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.debug("Closing block storage...")
        blockStorage.close()
      }
    })

    val storage = new Storage(blockStorage, settings)

    val validators = Seq(
      new DifficultyBlockValidator(storage)
      // fixme: JAA - 2020.07.19 - why are these commented out?
      //new ParentBlockValidator(storage),
      //new SemanticBlockValidator(FastCryptographicHash)
    )

    new History(storage, settings, validators)
  }
}
