package bifrost.history

import java.io.File

import bifrost.blocks.{BifrostBlock, Bloom}
import bifrost.forging.ForgingSettings
import bifrost.programBoxRegistry.ProgramBoxRegistryOld
import bifrost.validation.DifficultyBlockValidator
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import bifrost.NodeViewModifier
import bifrost.NodeViewModifier.{ModifierId, ModifierTypeId}
import bifrost.block.BlockValidator
import bifrost.consensus.History
import bifrost.consensus.History.{HistoryComparisonResult, ProgressInfo}
import bifrost.transaction.bifrostTransaction.BifrostTransaction
import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import bifrost.transaction.state.PrivateKey25519
import bifrost.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.collection.BitSet
import scala.util.{Failure, Try}

/**
  * A representation of the entire blockchain (whether it's a blocktree, blockchain, etc.)
  *
  * @param storage    a wrapper primarily for the LSMStore and for storage of the minimal state
  * @param settings   settings regarding updating forging difficulty, constants, etc.
  * @param validators rule sets that dictate validity of blocks in the history
  */
class BifrostHistory(val storage: BifrostStorage,
                     settings: ForgingSettings,
                     validators: Seq[BlockValidator[BifrostBlock]])
  extends History[ProofOfKnowledgeProposition[PrivateKey25519],
    BifrostTransaction,
    BifrostBlock,
    BifrostSyncInfo,
    BifrostHistory
    ] with ScorexLogging {

  override type NVCT = BifrostHistory

  require(NodeViewModifier.ModifierIdSize == 32, "32 bytes ids assumed")

  lazy val height: Long = storage.height
  lazy val score: Long = storage.bestChainScore
  lazy val bestBlockId: Array[Byte] = storage.bestBlockId
  lazy val difficulty: Long = storage.difficultyOf(bestBlockId).get
  lazy val bestBlock: BifrostBlock = storage.bestBlock


  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = height <= 0

  override def applicable(block: BifrostBlock): Boolean = {
    contains(block.parentId)
  }

  override def modifierById(id: ModifierId): Option[BifrostBlock] = storage.modifierById(id)

  override def contains(id: ModifierId): Boolean =
    if (id sameElements settings.GenesisParentId) true else modifierById(id).isDefined

  /**
    * Adds block to chain and updates storage (difficulty, score, etc.) relating to that
    *
    * @param block block to append
    * @return the update history including `block` as the most recent block
    */
  override def append(block: BifrostBlock):
  Try[(BifrostHistory, ProgressInfo[BifrostBlock])] = Try {

    log.debug(s"Trying to append block ${Base58.encode(block.id)} to history")
    val validationResults = validators.map(_.validate(block))

    validationResults.foreach {
      case Failure(e) => log.warn(s"Block validation failed", e)
      case _ =>
    }
    validationResults.foreach(_.get)

    val res: (BifrostHistory, ProgressInfo[BifrostBlock]) = {

      if (isGenesis(block)) {
        storage.update(block, settings.InitialDifficulty, isBest = true)
        val progInfo = ProgressInfo(None, Seq(), Seq(block))
        (new BifrostHistory(storage, settings, validators), progInfo)
      } else {
        val parent = modifierById(block.parentId).get
        val oldDifficulty = storage.difficultyOf(block.parentId).get
        var difficulty = (oldDifficulty * settings.targetBlockTime.length) / (block.timestamp - parent.timestamp)
        if (difficulty < settings.MinimumDifficulty) difficulty = settings.MinimumDifficulty
        val builtOnBestChain = applicable(block)
        // Check that the new block's parent is the last best block
        val mod: ProgressInfo[BifrostBlock] = if (!builtOnBestChain) {
          log.debug(s"New orphaned block ${Base58.encode(block.id)}")
          ProgressInfo(None, Seq(), Seq())
        } else if (block.parentId sameElements storage.bestBlockId) { // new block parent is best block so far
          log.debug(s"New best block ${Base58.encode(block.id)}")
          ProgressInfo(None, Seq(), Seq(block))
        } else { // we want to swap to a fork
          bestForkChanges(block)
        }
        storage.update(block, difficulty, builtOnBestChain)
        (new BifrostHistory(storage, settings, validators), mod)
      }
    }
    log.info(s"History: block ${Base58.encode(block.id)} appended to chain with score ${storage.scoreOf(block.id)}. " +
               s"Best score is $score. Pair: ${Base58.encode(bestBlockId)}")
    res
  }

  /**
    * Removes this block (and its children) from the history and rolls back to the state after the parent block
    *
    * @param modifierId the id of the block to chop off
    * @return a new history which is the current history ending at the parent of the dropped block
    */
  override def drop(modifierId: ModifierId): BifrostHistory = {

    val block = storage.modifierById(modifierId).get
    val parentBlock = storage.modifierById(block.parentId).get

    log.debug(s"Failed to apply block. Rollback BifrostState to ${Base58.encode(parentBlock.id)} from version ${
      Base58
        .encode(block.id)
    }")
    storage.rollback(parentBlock.id)
    new BifrostHistory(storage, settings, validators)
  }

  /**
    * Calculates the changes necessary for a swap to a fork
    *
    * @param block the block that has a parent on the canonical chain but is not on the chain
    * @return ProgressInfo that specifies the blocks to delete (worse fork) and blocks to add (better fork)
    */
  def bestForkChanges(block: BifrostBlock): ProgressInfo[BifrostBlock] = {

    /* Get the two branches of the fork including their common block */
    val (newSuffix, oldSuffix) = commonBlockThenSuffixes(modifierById(block.parentId).get)

    log.debug(s"Processing fork for block ${Base58.encode(block.id)}: \n" +
                s"old: ${oldSuffix.map(Base58.encode)}\n" +
                s"new: ${newSuffix.map(Base58.encode)}")

    /* Roll back to the common block */
    val rollbackPoint = newSuffix.headOption
    val throwBlocks = oldSuffix.tail.map(id => modifierById(id).get)

    /* Add the newSuffix to the rolled-back chain */
    val applyBlocks = newSuffix.tail.map(id => modifierById(id).get) ++ Seq(block)

    require(applyBlocks.nonEmpty)
    require(throwBlocks.nonEmpty)

    ProgressInfo[BifrostBlock](rollbackPoint, throwBlocks, applyBlocks)
  }

  private def bounded(value: BigInt, min: BigInt, max: BigInt): BigInt = max.min(value.max(min))

  /**
    * Forms a list of available blocks to build upon
    *
    * @return the blocks that are "exposed" for use
    */
  override def openSurfaceIds(): Seq[ModifierId] =
    if (isEmpty) {
      Seq(settings.GenesisParentId)
    } else {
      Seq(bestBlockId)
    } // TODO return sequence of exposed endpoints?


  /**
    * Gather blocks from after `from` that should be added to the chain
    *
    * @param from the list of known blocks from which to gather continuation
    * @param size the number of blocks to return after `from`
    * @return
    */
  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)],
                               size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = {

    /* Whether m is a genesis block or is in `from` */
    def inList(m: BifrostBlock): Boolean = idInList(m.id) || isGenesis(m)

    def idInList(id: ModifierId): Boolean = from.exists(f => f._2 sameElements id)

    /* Extend chain back until end of `from` is found, then return <size> blocks continuing from that point */
    chainBack(bestBlock, inList) match {
      case Some(chain) if chain.exists(id => idInList(id._2)) => Some(chain.take(size))
      case Some(chain) =>
        log.warn("Found chain without ids from remote")
        None
      case _ => None
    }
  }

  /**
    * Return specified number of Bifrost blocks, ordered back from last one
    *
    * @param count - how many blocks to return
    * @return PoW blocks, in reverse order (starting from the most recent one)
    */
  def lastBlocks(count: Int, startBlock: BifrostBlock): Seq[BifrostBlock] = if (isEmpty) {
    Seq()
  } else {
    @tailrec
    def loop(b: BifrostBlock, acc: Seq[BifrostBlock] = Seq()): Seq[BifrostBlock] = if (acc.length >= count) {
      acc
    } else {
      modifierById(b.parentId) match {
        case Some(parent: BifrostBlock) => loop(parent, b +: acc)
        case _ => b +: acc
      }
    }

    loop(startBlock)
  }

  override def syncInfo(answer: Boolean): BifrostSyncInfo =
    BifrostSyncInfo(answer, lastBlocks(BifrostSyncInfo.MaxLastBlocks, bestBlock).map(_.id), score)

  /**
    * Given a sequence of blocks, finds the subset of blocks that diverge from the local state's sequence. This works
    * back from the most recent block to earlier blocks until one is found that exists in both sequences.
    *
    * @param otherLastBlocks the sequence of blocks against which to compare the local list
    * @param suffixFound     the sequence of blocks so far that do not match the local state
    * @return the eventual sequence of blocks that differs, including the merge point block
    */
  @tailrec
  private def divergentSuffix(otherLastBlocks: Seq[ModifierId],
                              suffixFound: Seq[ModifierId] = Seq()): Seq[ModifierId] = {
    val head = otherLastBlocks.head
    val newSuffix = suffixFound :+ head
    modifierById(head) match {
      case Some(b) => newSuffix
      case None => if (otherLastBlocks.length <= 1) Seq() else divergentSuffix(otherLastBlocks.tail, newSuffix)
    }
  }

  /**
    * Whether another's node syncinfo shows that another node is ahead or behind ours
    *
    * @param other other's node sync info
    * @return Equal if nodes have the same history, Younger if another node is behind, Older if a new node is ahead
    */
  override def compare(other: BifrostSyncInfo): HistoryComparisonResult.Value = {

    val local = score
    val remote = other.score

    log.debug(s"Remote's score is: $remote, Local's score is: $local")
    if (local < remote) {
      HistoryComparisonResult.Older
    } else if (local == remote) {
      HistoryComparisonResult.Equal
    } else {
      HistoryComparisonResult.Younger
    }
  }

  private def isGenesis(b: BifrostBlock): Boolean = storage.isGenesis(b)

  def blockForger(m: BifrostBlock): PublicKey25519Proposition = m.forgerBox.proposition

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
    def loopBackAndIncrementForger(m: BifrostBlock): Unit = {
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

  def count(f: BifrostBlock => Boolean): Int = filter(f).length

  def filter(f: BifrostBlock => Boolean): Seq[BifrostBlock] = {
    @tailrec
    def loop(m: BifrostBlock, acc: Seq[BifrostBlock]): Seq[BifrostBlock] = parentBlock(m) match {
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
  def getBlockIdsByBloom(f: BitSet => Boolean): Seq[Array[Byte]] = {
    @tailrec
    def loop(current: Array[Byte], acc: Seq[Array[Byte]]): Seq[Array[Byte]] = storage.parentIdOf(current) match {
      case Some(value) =>
        if (f(storage.bloomOf(current).get)) loop(value, current +: acc) else loop(value, acc)
      case None =>
        if (f(storage.bloomOf(current).get)) current +: acc else acc
    }

    loop(storage.bestBlockId, Seq())
  }

  def bloomFilter(queryBloomTopics: IndexedSeq[Array[Byte]]): Seq[BifrostTransaction] = {
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

  def parentBlock(m: BifrostBlock): Option[BifrostBlock] = modifierById(m.parentId)

  /**
    * Go back through chain and get block ids until condition `until` is satisfied
    *
    * @param m     the modifier to start at
    * @param until the condition that indicates (when true) that recursion should stop
    * @param limit the maximum number of blocks to recurse back
    * @param acc   the aggregated chain so far
    * @return the sequence of block information (TypeId, Id) that were collected until `until` was satisfied
    *         (None only if the parent for a block was not found) starting from the original `m`
    */
  @tailrec
  final def chainBack(m: BifrostBlock,
                        until: BifrostBlock => Boolean,
                        limit: Int = Int.MaxValue,
                        acc: Seq[(ModifierTypeId, ModifierId)] = Seq()): Option[Seq[(ModifierTypeId, ModifierId)]] = {

    val sum: Seq[(ModifierTypeId, ModifierId)] = (BifrostBlock.ModifierTypeId -> m.id) +: acc

    /* Check if the limit has been reached or if condition satisfied */
    if (limit <= 0 || until(m)) {
      Some(sum)

    } else {
      parentBlock(m) match {
        case Some(parent) => chainBack(parent, until, limit - 1, sum)
        case _ =>
          log.warn(s"Parent block for ${Base58.encode(m.id)} not found ")
          None
      }
    }
  }

  /**
    * Find common suffixes for two chains starting from forkBlock
    *
    * @param forkBlock the common source of a potential fork (normally just the parent of bestBlock)
    * @param limit     how far back to check for a common block
    * @return sequences which contain the last common block and variant blocks for the chains
    */
  final def commonBlockThenSuffixes(forkBlock: BifrostBlock,
                                    limit: Int = Int.MaxValue): (Seq[ModifierId], Seq[ModifierId]) = {

    /* The entire chain that was "best" */
    val loserChain = chainBack(bestBlock, isGenesis, limit).get.map(_._2)

    /* `in` specifies whether `loserChain` has this block */
    def in(m: BifrostBlock): Boolean = loserChain.exists(s => s sameElements m.id)

    /* Finds the chain of blocks back from `forkBlock` until a common block to `loserChain` is found */
    val winnerChain = chainBack(forkBlock, in, limit).get.map(_._2)

    val i = loserChain.indexWhere(id => id sameElements winnerChain.head)

    /* The two segments including their common block */
    (winnerChain, loserChain.takeRight(loserChain.length - i))

  }.ensuring(r => r._1.head sameElements r._2.head)

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
    chainBack(bestBlock, isGenesis).get.map(_._2).map(Base58.encode).mkString(",")
  }

}


object BifrostHistory extends ScorexLogging {

  def readOrGenerate(settings: ForgingSettings): BifrostHistory = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get
    val logDirOpt = settings.logDirOpt
    readOrGenerate(dataDir, logDirOpt, settings)
  }

  def readOrGenerate(dataDir: String, logDirOpt: Option[String], settings: ForgingSettings): BifrostHistory = {
    val iFile = new File(s"$dataDir/blocks")
    iFile.mkdirs()
    val blockStorage = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing block storage...")
        blockStorage.close()
      }
    })

    val storage = new BifrostStorage(blockStorage, settings)

    val validators = Seq(
      new DifficultyBlockValidator(storage)
      //new ParentBlockValidator(storage),
      //new SemanticBlockValidator(FastCryptographicHash)
    )

    new BifrostHistory(storage, settings, validators)
  }
}