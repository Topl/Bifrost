package co.topl.consensus.tine

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}

import co.topl.models._
import co.topl.consensus.vrf.ProofToHash
import co.topl.consensus.TetraParameters
import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import scala.collection.immutable.ArraySeq

case class Tine(
  var best:        mutable.SortedMap[BigInt, SlotId] = mutable.SortedMap(),
  var maxSlot:     Option[Slot] = None,
  var minSlot:     Option[Slot] = None
)(implicit blocks: BlockStorage) {
  import Tine._

  val tineCacheSize = 12

  /**
   * LoadingCache for restoring segments of tine from the block database,
   * Max size is 12 to accommodate 2 epochs total,
   * best keeps track of the block Id with the highest block number in each block database
   */

  private lazy val tineCache: LoadingCache[BigInt, TineCache] = CacheBuilder
    .newBuilder()
    .maximumSize(tineCacheSize)
    .build[BigInt, TineCache](
      new CacheLoader[BigInt, TineCache] {

        def load(epoch3rd: BigInt): TineCache =
          if (best.keySet.contains(epoch3rd)) {
            val bestBlockId = best(epoch3rd)
            var out: TineCache = emptyTineCache
            var buildTine = true
            var testId = bestBlockId
            while (buildTine)
              blocks.restoreHeader(testId) match {
                case Some(header) =>
                  out = prepend(
                    out,
                    (header.slot, testId._2, ProofToHash.digest(header.eligibibilityCertificate.vrfNonceSig))
                  )
                  if (header.parentSlot < minSlot.get || BigInt(header.parentSlot / databaseInterval) != epoch3rd) {
                    buildTine = false
                  } else {
                    testId = (header.parentSlot, header.parentHeaderId)
                  }
                case None => buildTine = false
              }
            out
          } else {
            emptyTineCache
          }
      }
    )

  /**
   * Either a single Array database or a LoadingCache of Array databases,
   * loading cache can restore a segment of tine from the block database,
   * Initializing tineCache is resource intensive so the database is handled with Either Left Right logic,
   * The database is started Left with a single cache for all slots,
   * Once the Left database expands the update method will turn it Right and load the tineCache
   */

  private var tineDB: Either[TineCache, LoadingCache[BigInt, TineCache]] = Left(emptyTineCache)

  /**
   * Set up the database to use the loading cache
   */

  def loadCache(): Unit =
    tineDB match {
      case Left(cache) if cache.nonEmpty =>
        tineDB = Right(tineCache)
        minSlot = None
        maxSlot = None
        best = mutable.SortedMap()
        cache.foreach(entry => this.update((entry._1, entry._2), entry._3))
      case _ =>
        tineDB = Right(tineCache)
    }

  /**
   * Loads data into the tine during initialization, and populates the blocks database cache with latest blocks
   */

  def populateCache(): Unit =
    tineDB match {
      case Left(_) => assert(false)
      case Right(loadingCache) =>
        assert(best.nonEmpty)
        val indexBest = best.keySet.max
        for (i <- BigInt(Array(0, indexBest.toInt - tineCacheSize).max) to indexBest)
          loadingCache.refresh(i)
        blocks.populateCache(best(indexBest))
    }

  /**
   * Primary means of adding slotIds to the tine
   * @param slotId new id to add
   * @param nonce nonce data
   */

  def update(slotId: SlotId, nonce: Rho): Unit = Try {
    val newEntry = (slotId._1, slotId._2, nonce)
    tineDB match {
      case Left(cache) =>
        if (cache.isEmpty) {
          maxSlot = Some(slotId._1)
          minSlot = Some(slotId._1)
          tineDB = Left(append(cache, newEntry))
        } else {
          maxSlot match {
            case Some(slot) if slotId._1 > slot =>
              assert(toSlotId(cache.last) == blocks.getHeader(slotId).get.parentSlotId)
              tineDB = Left(append(cache, newEntry))
              maxSlot = Some(slotId._1)
            case _ =>
          }
          minSlot match {
            case Some(slot) if slotId._1 < slot =>
              assert(slotId == blocks.getHeader(toSlotId(cache.head)).get.parentSlotId)
              tineDB = Left(prepend(cache, newEntry))
              minSlot = Some(slotId._1)
            case _ =>
          }
        }
      case Right(loadingCache) =>
        val index = slotId._1 / databaseInterval
        val cacheKey = BigInt(index)
        val cache: TineCache = loadingCache.get(cacheKey)
        var wasUpdated = false
        maxSlot match {
          case Some(slot) if slotId._1 > slot =>
            if (cache.isEmpty) {
              if (index > 0) assert(best(BigInt(index - 1)) == blocks.getHeader(slotId).get.parentSlotId)
            } else {
              assert(toSlotId(cache.last) == blocks.getHeader(slotId).get.parentSlotId)
            }
            loadingCache.invalidate(cacheKey)
            loadingCache.put(cacheKey, append(cache, newEntry))
            maxSlot = Some(slotId._1)
            wasUpdated = true
          case Some(slot) if slotId._1 <= slot =>
          case None =>
            loadingCache.invalidate(cacheKey)
            assert(cache.isEmpty)
            loadingCache.put(cacheKey, append(cache, newEntry))
            maxSlot = Some(slotId._1)
            wasUpdated = true
          case _ => assert(false)
        }
        minSlot match {
          case Some(slot) if slotId._1 < slot =>
            if (cache.nonEmpty) {
              assert(slotId == blocks.getHeader(toSlotId(cache.head)).get.parentSlotId)
            } else {
              assert(slotId == blocks.getHeader(toSlotId(loadingCache.get(BigInt(index + 1)).head)).get.parentSlotId)
            }
            loadingCache.invalidate(cacheKey)
            loadingCache.put(cacheKey, prepend(cache, newEntry))
            minSlot = Some(slotId._1)
            wasUpdated = true
          case Some(slot) if slotId._1 >= slot =>
          case None =>
            assert(cache.isEmpty)
            assert(wasUpdated)
            minSlot = Some(slotId._1)
          case _ => assert(false)
        }
        best.get(cacheKey) match {
          case Some(bestId) if slotId._1 >= bestId._1 =>
            best -= cacheKey
            best += (cacheKey -> slotId)
          case Some(bestId) if slotId._1 < bestId._1 =>
          case None =>
            assert(cache.isEmpty)
            best += (cacheKey -> slotId)
          case _ => assert(false)
        }
        assert(wasUpdated)
    }
  } match {
    case Failure(e) => e.printStackTrace()
    case _          =>
  }

  /**
   * Primary means of updating a tine during chain selection,
   * cleans the database of any slotIds to be removed,
   * updates this with the slotIds from tine
   * @param prefix common prefix slot of this and tine
   * @param tine tine containing slotIds to add to this
   */

  def reorg(prefix: Slot, tine: Tine): Unit = Try {
    if (maxSlot.get == prefix && prefix < tine.minSlot.get) {
      assert(this.head == blocks.getHeader(tine.oldest).get.parentSlotId)
      for (id <- tine.ordered)
        this.update(id, tine.getNonce(id._1).get)
    } else {
      tineDB match {
        case Left(cache) =>
          assert(best.isEmpty)
          assert(cache.nonEmpty)
          val newCache = cache.filter(data => data._1 <= prefix)
          tineDB = Left(newCache)
          val newMax = newCache.last._1
          maxSlot = Some(newMax)
          assert(this.head == blocks.getHeader(tine.oldest).get.parentSlotId)
          for (id <- tine.ordered)
            this.update(id, tine.getNonce(id._1).get)
        case Right(cache) =>
          val prefixKey = BigInt(prefix / databaseInterval)
          val newCache = cache.get(prefixKey).filter(data => data._1 <= prefix)
          best.keySet.filter(key => key >= prefixKey).foreach(key => cache.invalidate(key))
          best = best.filter(data => data._1 < prefixKey)
          cache.put(prefixKey, newCache)
          val newMax = newCache.last._1
          maxSlot = Some(newMax)
          val newBest: SlotId = toSlotId(newCache.last)
          best += (prefixKey -> newBest)
          assert(this.head == blocks.getHeader(tine.oldest).get.parentSlotId)
          for (id <- tine.ordered)
            this.update(id, tine.getNonce(id._1).get)
      }
    }
  } match {
    case Failure(exception) => exception.printStackTrace()
    case _                  =>
  }

  /**
   * Checks if there is an active slot past prefix within the slotWindow,
   * tine is invalid if there are gaps longer than slotWindow
   * @param prefix starting slot to begin search
   * @return true if tine dense, false otherwise
   */

  def notSparsePast(prefix: Slot): Boolean = {
    var foundSlot = false
    tineDB match {
      case Left(cache) =>
        cache.find(data => data._1 > prefix && data._1 < prefix + slotWindow) match {
          case None =>
          case _    => foundSlot = true
        }
      case Right(cache) =>
        for (index <- (prefix + 1) / databaseInterval to (prefix + slotWindow) / databaseInterval)
          if (!foundSlot) {
            val cacheKey = BigInt(index)
            cache.get(cacheKey).find(data => data._1 > prefix && data._1 < prefix + slotWindow) match {
              case None =>
              case _    => foundSlot = true
            }
          }
    }
    foundSlot
  }

  /**
   * Primary means of calculating epoch nonce data,
   * Includes optional tine in case a reorg is happening across epochs
   * @param min minimum slot to collect nonces
   * @param max maximum slot to collect nonces
   * @param tine tine to load nonces from if window of min and max requires nonces from this and tine
   * @return serial nonces for epoch nonce calculation
   */

  def orderedNonceData(min: Slot, max: Slot, tine: Option[Tine]): Bytes =
    if (min < max) {
      tine match {
        case Some(t) if t.minSlot.get <= min =>
          t.orderedNonceData(min, max, None)
        case Some(t) if t.minSlot.get <= max =>
          tineDB match {
            case Left(cache) =>
              val filtered = cache.filter(data => data._1 < t.minSlot.get && data._1 >= min).toSeq
              Bytes.concat(
                Bytes.concat(filtered.map(entry => entry._3.data): _*),
                t.orderedNonceData(t.minSlot.get, max, None)
              )
            case Right(cache) =>
              var out: Bytes = Bytes.empty
              for (index <- min / databaseInterval to (t.minSlot.get - 1) / databaseInterval) {
                val cacheKey = BigInt(index)
                val newCache = cache.get(cacheKey).filter(data => data._1 < t.minSlot.get && data._1 >= min)
                out = Bytes.concat(out, Bytes.concat(newCache.map(entry => entry._3.data).toIndexedSeq: _*))
              }
              Bytes.concat(out, t.orderedNonceData(t.minSlot.get, max, tine))
          }
        case _ =>
          tineDB match {
            case Left(cache) =>
              val newCache = cache.filter(data => data._1 <= max && data._1 >= min)
              Bytes.concat(newCache.map(entry => entry._3.data).toIndexedSeq: _*)
            case Right(cache) =>
              var out: Bytes = Bytes.empty
              for (index <- min / databaseInterval to max / databaseInterval) {
                val cacheKey = BigInt(index)
                val newCache = cache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
                out = Bytes.concat(out, Bytes.concat(newCache.map(entry => entry._3.data).toIndexedSeq: _*))
              }
              out
          }
      }
    } else if (min == max) {
      tine match {
        case Some(t) if t.minSlot.get <= max =>
          t.getNonce(max) match {
            case Some(nonce) => nonce.data
            case None        => Bytes.empty
          }
        case _ =>
          this.getNonce(max) match {
            case Some(nonce) => nonce.data
            case None        => Bytes.empty
          }
      }
    } else {
      Bytes.empty
    }

  /**
   * Get the slotId of a given slot
   * @param slot given slot
   * @return slotId or None if not found
   */

  def get(slot: Slot): Option[SlotId] =
    tineDB match {
      case Left(cache) =>
        cache.find(entry => entry._1 == slot) match {
          case Some(data) => Some((slot, data._2))
          case None       => None
        }
      case Right(cache) =>
        val cacheKey = BigInt(slot / databaseInterval)
        cache.get(cacheKey).find(entry => entry._1 == slot) match {
          case Some(data) => Some((slot, data._2))
          case None       => None
        }
    }

  /**
   * Get the nonce of a given slot
   * @param slot given slot
   * @return nonce of that slot or None if not found
   */

  def getNonce(slot: Slot): Option[Rho] =
    tineDB match {
      case Left(cache) =>
        cache.find(entry => entry._1 == slot) match {
          case Some(data) => Some(data._3)
          case None       => None
        }
      case Right(cache) =>
        val cacheKey = BigInt(slot / databaseInterval)
        cache.get(cacheKey).find(entry => entry._1 == slot) match {
          case Some(data) => Some(data._3)
          case None       => None
        }
    }

  /**
   * Return up to n blocks past the starting slot,
   * Starting slot is included in returned array
   * @param start starting slot to begin search
   * @param n number of ids to return
   * @return set of ids in ascending block number order
   */

  def getNext(start: Slot, n: Int): Array[SlotId] =
    tineDB match {
      case Left(cache) =>
        var out: Array[SlotId] = Array()
        val filtered = cache.filter(entry => entry._1 >= start)
        filtered.foreach(entry =>
          if (out.length < n) {
            out = out ++ Array(toSlotId(entry))
          }
        )
        out
      case Right(cache) =>
        var out: Array[SlotId] = Array()
        var index = start / databaseInterval
        var done = false
        while (!done) {
          val cacheKey = BigInt(index)
          if (best.keySet.contains(cacheKey)) {
            val filtered = cache.get(cacheKey).filter(entry => entry._1 >= start)
            filtered.foreach(entry =>
              if (out.length < n) {
                out = out ++ Array(toSlotId(entry))
              } else {
                done = true
              }
            )
            index += 1
          } else {
            done = true
          }
        }
        out
    }

  /**
   * Gets the slotId at or just below the given slot
   * @param slot test slot to start search
   * @return next lower slotId up to and including slot or None if not found
   */

  def getLastActiveSlot(slot: Slot): Option[SlotId] = get(lastActiveSlot(slot).get)

  /**
   * Gets the slot at or just below the given slot
   * @param slot test slot to start search
   * @return next lower slot up to and including slot or None if not found
   */

  def lastActiveSlot(slot: Slot): Option[Slot] =
    if (slot >= maxSlot.get) {
      maxSlot
    } else if (slot == minSlot.get) {
      minSlot
    } else if (slot < minSlot.get) {
      minSlot
    } else {
      tineDB match {
        case Left(cache) =>
          Try {
            cache.filter(data => data._1 <= slot).last._1
          }.toOption
        case Right(cache) =>
          var index = slot / databaseInterval
          var done = false
          var out: Option[Slot] = None
          while (!done) {
            val cacheKey = BigInt(index)
            cache.get(cacheKey).filter(data => data._1 <= slot) match {
              case filtered if filtered.isEmpty =>
                if (index > 0) {
                  index -= 1
                } else {
                  done = true
                }
              case filtered if filtered.nonEmpty =>
                out = Some(filtered.last._1)
                done = true
            }
          }
          out
      }
    }

  /**
   * Total number of active slots
   * @return
   */

  def numActive: Int =
    maxSlot match {
      case None => 0
      case Some(max) =>
        minSlot match {
          case None => 0
          case Some(min) =>
            if (min == max) {
              1
            } else {
              tineDB match {
                case Left(cache) => cache.length
                case Right(cache) =>
                  var out = 0
                  for (index <- best.keySet)
                    out += cache.get(index).length
                  out
              }
            }
        }
    }

  /**
   * head of the tine, highest slot
   * @return head slotId
   */

  def head: SlotId =
    get(maxSlot.get).get

  /**
   * beginning of the tine, lowest slot
   * @return oldest slotId
   */

  def oldest: SlotId =
    get(minSlot.get).get

  /**
   * Sub-Tine with slots in between and including min and max
   * @param min minimum slot
   * @param max maximum slot
   * @return tine with minSlot >= min and maxSlot <= max
   */

  def slice(min: Slot, max: Slot): Tine =
    if (min < max) {
      tineDB match {
        case Left(cache) =>
          val out = new Tine
          val newCache = cache.filter(data => data._1 <= max && data._1 >= min)
          out.tineDB = Left(newCache)
          out.minSlot = Try(newCache.head._1).toOption
          out.maxSlot = Try(newCache.last._1).toOption
          out
        case Right(cache) =>
          if (max - min > slotWindow) {
            var minOut: Option[Slot] = None
            var maxOut: Option[Slot] = None
            val out = new Tine
            for (index <- min / databaseInterval to max / databaseInterval) {
              val cacheKey = BigInt(index)
              val newCache = cache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
              if (newCache.nonEmpty) {
                val cacheMin = newCache.head._1
                val cacheMax = newCache.last._1
                minOut = minOut match {
                  case None => Some(cacheMin)
                  case _    => Some(Array(cacheMin, minOut.get).min)
                }
                maxOut = maxOut match {
                  case None => Some(cacheMax)
                  case _    => Some(Array(cacheMax, maxOut.get).max)
                }
                val bestId: SlotId = toSlotId(newCache.last)
                out.best += (cacheKey -> bestId)
                out.loadCache()
                out.tineCache.put(cacheKey, newCache)
              }
            }
            out.minSlot = minOut
            out.maxSlot = maxOut
            out
          } else {
            var minOut: Option[Slot] = None
            var maxOut: Option[Slot] = None
            val out = new Tine
            var outCache: TineCache = emptyTineCache
            if (!this.isEmpty) for (index <- min / databaseInterval to max / databaseInterval) {
              val cacheKey = BigInt(index)
              val newCache = cache.get(cacheKey).filter(data => data._1 <= max && data._1 >= min)
              if (newCache.nonEmpty) {
                val cacheMin = newCache.head._1
                val cacheMax = newCache.last._1
                minOut = minOut match {
                  case None => Some(cacheMin)
                  case _    => Some(Array(cacheMin, minOut.get).min)
                }
                maxOut = maxOut match {
                  case None => Some(cacheMax)
                  case _    => Some(Array(cacheMax, maxOut.get).max)
                }
                outCache ++= newCache
              }
            }
            out.tineDB = Left(outCache)
            out.minSlot = minOut
            out.maxSlot = maxOut
            out
          }
      }
    } else if (min == max) {
      val out = new Tine
      this.get(max) match {
        case None =>
        case Some(id) =>
          this.getNonce(max) match {
            case None        =>
            case Some(nonce) => out.update(id, nonce)
          }
      }
      out
    } else {
      new Tine
    }

  private def toSlotId(data: (Slot, TypedIdentifier, Rho)): SlotId = (data._1, data._2)

  def ordered: Array[SlotId] = {
    var out: Array[SlotId] = Array()
    tineDB match {
      case Left(cache) =>
        cache.map(toSlotId)
      case Right(cache) =>
        for (index <- best.keySet)
          out = out ++ cache.get(index).map(toSlotId)
        out
    }
  }

  def isEmpty: Boolean =
    tineDB match {
      case Left(cache) =>
        cache.isEmpty
      case Right(_) =>
        best.isEmpty
    }

  def copy(): Tine = {
    val out: Tine = new Tine
    out.minSlot = minSlot
    out.maxSlot = maxSlot
    out.best = best
    tineDB match {
      case Left(cache) =>
        out.tineDB = Left(cache)
      case Right(cache) =>
        out.loadCache()
        cache.asMap
          .keySet()
          .forEach(key =>
            cache.getIfPresent(key) match {
              case value: TineCache => out.tineCache.put(key, value)
              case _                =>
            }
          )
    }
    out
  }

  def copy(tine: Tine): Unit =
    tineDB match {
      case Left(_) =>
        assert(false)
      case Right(_) =>
        this.minSlot = tine.minSlot
        this.maxSlot = tine.maxSlot
        this.best = tine.best
    }

  def verify: Boolean = Try {
    tineDB match {
      case Left(cache) =>
        assert(best.isEmpty)
        if (cache.nonEmpty) {
          assert(minSlot.get == cache.head._1)
          assert(maxSlot.get == cache.last._1)
          var id: SlotId = toSlotId(cache.last)
          var block: BlockHeaderV2 = blocks.getHeader(id).get
          var nonce: Rho = ProofToHash.digest(block.eligibibilityCertificate.vrfNonceSig)
          assert(nonce == cache.last._3)
          for (entry <- cache.reverse.tail) {
            val pid = block.parentSlotId
            assert(toSlotId(entry) == pid)
            id = pid
            block = blocks.getHeader(id).get
            nonce = ProofToHash.digest(block.eligibibilityCertificate.vrfNonceSig)
            assert(nonce == entry._3)
          }
        } else {
          assert(maxSlot.isEmpty)
          assert(minSlot.isEmpty)
        }
      case Right(loaderCache) =>
        if (best.isEmpty) {
          assert(maxSlot.isEmpty)
          assert(minSlot.isEmpty)
        } else {
          var id: SlotId = best(best.keySet.max)
          var cachePid: Option[SlotId] = None
          assert(id._1 == maxSlot.get)
          for (value <- best.toArray.reverse) {
            val cache = loaderCache.get(value._1)
            id = toSlotId(cache.last)
            assert(cachePid match {
              case None      => true
              case Some(cid) => cid == id
            })
            assert(id == best(value._1))
            var block: BlockHeaderV2 = blocks.getHeader(id).get
            var nonce: Rho = ProofToHash.digest(block.eligibibilityCertificate.vrfNonceSig)
            assert(nonce == cache.last._3)
            for (entry <- cache.reverse.tail) {
              val pid = block.parentSlotId
              assert(toSlotId(entry) == pid)
              id = pid
              block = blocks.getHeader(id).get
              nonce = ProofToHash.digest(block.eligibibilityCertificate.vrfNonceSig)
              assert(nonce == entry._3)
            }
            if (id._1 > 0) cachePid = Some(blocks.getHeader(id).get.parentSlotId)
          }
        }
    }
  } match {
    case Success(_) => true
    case Failure(exception) =>
      exception.printStackTrace()
      false
  }

  def slotIntervalDist: mutable.SortedMap[Int, Int] = {
    val out: mutable.SortedMap[Int, Int] = mutable.SortedMap()
    this.ordered.foreach { id =>
      val header = blocks.getHeader(id).get
      val interval = (header.slot - header.parentSlot).toInt
      if (header.slot > 0) {
        out.get(interval) match {
          case Some(i) => out.update(interval, i + 1)
          case None    => out.update(interval, 1)
        }
      }
    }
    out
  }

}

object Tine {

  type TineData = (mutable.SortedMap[BigInt, SlotId], Slot, Slot)

  val databaseInterval: Slot = TetraParameters.one_third_epoch
  val slotWindow: Slot = TetraParameters.slotWindow

  type SlotData = (Slot, TypedIdentifier, Rho)
  type TineCache = Array[SlotData]

  def emptyTineCache: TineCache = Array.empty

  def append(tineCache: TineCache, input: SlotData): TineCache =
    tineCache ++ Array(input)

  def prepend(tineCache: TineCache, input: SlotData): TineCache =
    Array(input) ++ tineCache

  def apply()(implicit blocks: BlockStorage): Tine = new Tine

  //for new heads in tinePool
  def apply(id: SlotId, nonce: Rho)(implicit blocks: BlockStorage): Tine = {
    val out = new Tine
    out.update(id, nonce)
    out
  }

  //for loading localChain data from disk
  def apply(data: TineData)(implicit blocks: BlockStorage): Tine = {
    val out = new Tine
    out.loadCache()
    out.best = data._1
    out.minSlot = Some(data._2)
    out.maxSlot = Some(data._3)
    out
  }

}
