package examples.bifrost.history

import java.io.File
import java.time.Instant

import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.forging.{ForgingConstants, ForgingSettings}
import examples.bifrost.transaction.BifrostTransaction
import examples.bifrost.validation.DifficultyBlockValidator
import io.iohk.iodb.LSMStore
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.{Block, BlockValidator}
import scorex.core.consensus.History
import scorex.core.consensus.History.{HistoryComparisonResult, ProgressInfo}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
import scorex.core.transaction.state.PrivateKey25519
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.annotation.tailrec
import scala.util.{Failure, Try}

/**
  * History storage
  * we store all the blocks, even if they are not in a main chain
  */
class BifrostHistory(storage: BifrostStorage, settings: ForgingConstants, validators: Seq[BlockValidator[BifrostBlock]])
  extends History[ProofOfKnowledgeProposition[PrivateKey25519], BifrostTransaction, BifrostBlock, BifrostSyncInfo, BifrostHistory] with ScorexLogging {

  override type NVCT = BifrostHistory

  require(NodeViewModifier.ModifierIdSize == 32, "32 bytes ids assumed")

  val height: Long = storage.height
  val score: Long = storage.bestChainScore
  val bestBlockId: Array[Byte] = storage.bestBlockId
  val bestBlock: BifrostBlock = storage.bestBlock


  /**
    * Is there's no history, even genesis block
    *
    * @return
    */
  override def isEmpty: Boolean = height <= 0

  override def modifierById(id: ModifierId): Option[BifrostBlock] = storage.modifierById(id)

  override def contains(id: ModifierId): Boolean =
    if (id sameElements settings.GenesisParentId) true else modifierById(id).isDefined

  /**
    *
    * @param block - block to append
    * @return
    */
  override def append(block: BifrostBlock):
  Try[(BifrostHistory, ProgressInfo[BifrostBlock])] = Try {

    log.debug(s"Trying to append block ${Base58.encode(block.id)} to history")
    val validationResuls = validators.map(_.validate(block))

    validationResuls.foreach {
      case Failure(e) => log.warn(s"Block validation failed", e)
      case _ =>
    }

    validationResuls.foreach(_.get)

    val res: (BifrostHistory, ProgressInfo[BifrostBlock]) = {

      val parent = modifierById(block.parentId).get

      val oldDifficulty = storage.difficultyOf(block.parentId).get
      val difficulty = oldDifficulty * settings.targetBlockDelay / (block.timestamp - parent.timestamp)

      val builtOnBestChain = score == storage.parentChainScore(block)

      // Check that the new block's parent is the last best block
      val mod: ProgressInfo[BifrostBlock] = if(!builtOnBestChain) {
        log.debug(s"New orphaned block ${Base58.encode(block.id)}")
        ProgressInfo(None, Seq(), Seq())
      } else if (block.parentId sameElements storage.bestBlockId) { // new block parent is best block so far
        log.debug(s"New best block ${Base58.encode(block.id)}")
        ProgressInfo(None, Seq(), Seq(block))
      } else {
        bestForkChanges(block)
      }

      storage.update(block, Some(difficulty), builtOnBestChain)

      (new BifrostHistory(storage, settings, validators), mod)
    }
    log.info(s"History: block ${Base58.encode(block.id)} appended to chain with score ${storage.scoreOf(block.id)}. " +
      s"Best score is ${score}. " +
      s"Pair: ${Base58.encode(bestBlockId)}")
    res
  }

  //todo: implement
  override def drop(modifierId: ModifierId): BifrostHistory = ???

  def bestForkChanges(block: BifrostBlock): ProgressInfo[BifrostBlock] = {
    val (newSuffix, oldSuffix) = commonBlockThenSuffixes(modifierById(block.parentId).get)
    log.debug(s"Processing fork for block ${Base58.encode(block.id)}: \n" +
      s"old: ${oldSuffix.map(Base58.encode)}\n" +
      s"new: ${newSuffix.map(Base58.encode)}")

    val rollbackPoint = newSuffix.headOption

    val throwBlocks = oldSuffix.tail.map(id => modifierById(id).get)
    val applyBlocks = newSuffix.tail.map(id => modifierById(id).get) ++ Seq(block)
    require(applyBlocks.nonEmpty)
    require(throwBlocks.nonEmpty)

    ProgressInfo[BifrostBlock](rollbackPoint, throwBlocks, applyBlocks)
  }

  private def bounded(value: BigInt, min: BigInt, max: BigInt): BigInt = {
    if (value < min) min else if (value > max) max else value
  }

  override def openSurfaceIds(): Seq[ModifierId] =
    if (isEmpty) Seq(settings.GenesisParentId)
    else Seq(bestBlockId) // TODO return sequence of exposed endpoints?

  override def continuationIds(from: Seq[(ModifierTypeId, ModifierId)],
                               size: Int): Option[Seq[(ModifierTypeId, ModifierId)]] = {

    def inList(m: BifrostBlock): Boolean = idInList(m.id) || isGenesis(m)
    def idInList(id: ModifierId): Boolean = from.exists(f => f._2 sameElements id)

    // Extend chain back until end of from is found, then return <size> blocks continuing from that point
    chainBack(bestBlock, inList) match {
      case Some(chain) if chain.exists(id => idInList(id._2)) => Some(chain.take(size))
      case Some(chain) =>
        log.warn("Found chain without ids from remote")
        None
      case _ => None
    }
  }

  override def syncInfo(answer: Boolean): BifrostSyncInfo =
    BifrostSyncInfo(answer, bestBlockId, score)

  @tailrec
  private def divergentSuffix(otherLastBlocks: Seq[ModifierId], suffixFound: Seq[ModifierId] = Seq()): Seq[ModifierId] = {
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

    if (local < remote) HistoryComparisonResult.Older
    else if (local == remote) HistoryComparisonResult.Equal
    else HistoryComparisonResult.Younger

  }

  private def isGenesis(b: BifrostBlock): Boolean = storage.isGenesis(b)

  def blockGenerator(m: BifrostBlock): PublicKey25519Proposition = m.generatorBox.proposition

  def generatorDistribution(): Map[PublicKey25519Proposition, Int] = {
    val map = collection.mutable.Map[PublicKey25519Proposition, Int]().withDefaultValue(0)
    @tailrec
    def loop(m: BifrostBlock): Unit = {
      val generator = blockGenerator(m)
      map.update(generator, map(generator) + 1)
      parentBlock(m) match {
        case Some(parent) => loop(parent)
        case None =>
      }
    }
    loop(bestBlock)
    map.toMap
  }

  def count(f: (BifrostBlock => Boolean)): Int = filter(f).length

  def filter(f: (BifrostBlock => Boolean)): Seq[ModifierId] = {
    @tailrec
    def loop(m: BifrostBlock, acc: Seq[ModifierId]): Seq[ModifierId] = parentBlock(m) match {
      case Some(parent) => if (f(m)) loop(parent, m.id +: acc) else loop(parent, acc)
      case None => if (f(m)) m.id +: acc else acc
    }
    loop(bestBlock, Seq())
  }

  def parentBlock(m: BifrostBlock): Option[BifrostBlock] = modifierById(m.parentId)

  /**
    * Go back though chain and get block ids until condition until
    */
  @tailrec
  private def chainBack(m: BifrostBlock,
                        until: BifrostBlock => Boolean,
                        limit: Int = Int.MaxValue,
                        acc: Seq[(ModifierTypeId, ModifierId)] = Seq()): Option[Seq[(ModifierTypeId, ModifierId)]] = {

    val sum: Seq[(ModifierTypeId, ModifierId)] = (BifrostBlock.ModifierTypeId -> m.id) +: acc

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
    * find common suffixes for two chains - starting from forkBlock and from bestPowBlock
    * returns last common block and then variant blocks for two chains,
    */
  final def commonBlockThenSuffixes(forkBlock: BifrostBlock,
                                    limit: Int = Int.MaxValue): (Seq[ModifierId], Seq[ModifierId]) = {

    val loserChain = chainBack(bestBlock, isGenesis, limit).get.map(_._2)

    def in(m: BifrostBlock): Boolean = loserChain.exists(s => s sameElements m.id)

    val winnerChain = chainBack(forkBlock, in, limit).get.map(_._2)

    val i = loserChain.indexWhere(id => id sameElements winnerChain.head)
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

  def readOrGenerate(dataDir: String, logDirOpt: Option[String], settings: ForgingConstants): BifrostHistory = {
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