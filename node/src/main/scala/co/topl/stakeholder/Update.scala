package co.topl.stakeholder

import java.io.{BufferedWriter, File, FileWriter}

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.iohk.iodb.ByteArrayWrapper
import co.topl.cases.{Flag, WriteFile}
import co.topl.components.Tine
import co.topl.primitives.{KeyFile, Ratio, SharedData}
import co.topl.primitives.Base58

import scala.util.Try

/**
  * AMS 2020:
  * All methods required to update the Node View of Stakeholder to the latest global slot provided by Coordinator
  */

trait Update extends Members {

  /**
    * Epoch update routine, called every time currentEpoch increments, calculates the new epoch nonce
    * @param slot the slot being tested for update
    * @param epochIn the epoch number before slot is tested
    * @param lastEta the epoch nonce before slot is tested
    * @param chain the tine with vrf nonces to apply
    * @return epoch after slot is tested, epoch nonce after slot is tested
    */

  def updateEpoch(slot:Slot,epochIn:Int,lastEta:Eta,chain:Tine,tine:Option[Tine]):(Int,Eta) = {
    val ep = slot / epochLength
    if (ep > epochIn) {
      val newEta = eta_from_tine(chain, ep, lastEta, tine)
      (epochIn+1,newEta)
    } else {
      (epochIn,lastEta)
    }
  }

  /*********************************************************************************************************************
    * The staking distribution that is used to calculate the epoch relative stake,
    * Used in the staking procedure, the previous epoch is ep-1,
    * This staking distribution is sampled form the state at the beginning of the previous epoch or the genesis block,
    * This staking state results from all modifiers applied from the epoch leading into the previous epoch, i.e. ep-2,
    * The routine gets the last active slot starting just below the dawn slot of the previous epoch, and returns
    * the state with that block applied.
    *
    * @param ep epoch number corresponding to the returned staking distribution
    * @param chain tine containing the block ids of at least the previous epoch
    * @return the staking distribution to be used in epoch number ep
    */

  def getStakingState(ep:Int, chain:Tine, tine:Option[Tine]):State = if (ep > 1) {
    val stakeDistMaxSlot:Slot = (ep-1)*epochLength-1
    val stakeDistId:SlotId = localChain.getLastActiveSlot(stakeDistMaxSlot).get
    tine match {
      case Some(t) if t.minSlot.get <= stakeDistMaxSlot =>
        history.get(t.getLastActiveSlot(stakeDistMaxSlot).get) match {
          case Some(value:(State,Eta)) =>
            value._1
          case _ =>
            val thisSlot:Slot = t.lastActiveSlot(stakeDistMaxSlot).get
            println(s"Could not recover staking state ep $ep slot $thisSlot id:"
              +Base58.encode(localChain.getLastActiveSlot(stakeDistMaxSlot).get._2.data)
              +" from tine")
            SharedData.throwError(holderIndex)
            Map()
        }
      case _ =>
        history.getStakeDist(stakeDistId)
    }
  } else {
    history.getStakeDist((0,genBlockHash))
  }

  /*********************************************************************************************************************
    * The main update procedure that carries out consensus and forges, called up to 100 times a second
    *
    * This should execute the procedure given by LedgerMaintenance given in Ouroboros Genesis
    *
    * localSlot is used to keep track of epoch updates,
    * it updates to globalSlot in a while loop and triggers updateEpoch,
    * this allows the protocol to execute under stalls that delay queries to the Coordinator for time updates.
    *
    * Once local slot has reached global slot, and all epoch variables are updated, update the key to global slot.
    *
    * The staking procedure (testing the nonce if then forging) is carried out
    * even if the current set of keys has no stake, there is virtually no cost to forging
    * so it always occurs if active stake is held by the keys.
    * Newly forged blocks are added to the end of tinePoolWithPrefix so MaxValid-BG will adopt it first
    *
    * The new key is saved to disk, the keys already on disk are made old keys, and the old old key file is erased,
    * leaving only new key and old key on disk.
    *
    * Chain selection according to MaxValid-BG occurs on the last element of tinePoolWithPrefix
    *
    * Aux information is updated
    */

  def update():Unit = timeFlag{
    if (SharedData.error) actorStalled = true
    if (!actorStalled && !updating && !helloLock) {
      updating = true
      if (SharedData.killFlag) {
        timers.cancelAll
        updating = false
      }
      while (globalSlot > localSlot) {
        localSlot += 1
        if (dataOutFlag && localSlot % dataOutInterval == 0) {
          coordinatorRef ! WriteFile
        }
        updateEpoch(localSlot,currentEpoch,eta,localChain,None) match {
          case result:(Int,Eta) if result._1 > currentEpoch =>
            currentEpoch = result._1
            history.cacheStakeDist(localChain.best(BigInt(currentEpoch-1)))
            eta = result._2
            stakingState = getStakingState(currentEpoch,localChain,None)
            alphaCache match {
              case Some(loadingCache:LoadingCache[ByteArrayWrapper,Ratio]) =>
                loadingCache.invalidateAll()
              case None => alphaCache = Some(
                CacheBuilder.newBuilder().build[ByteArrayWrapper,Ratio](
                  new CacheLoader[ByteArrayWrapper,Ratio] {
                    def load(id:ByteArrayWrapper):Ratio = {relativeStake(id,stakingState)}
                  }
                )
              )
            }
            thresholdCache match {
              case Some(loadingCache: LoadingCache[(Ratio,Slot),Ratio]) => loadingCache.invalidateAll()
              case None =>
            }
            keys.alpha = alphaCache.get.get(keys.pkw)
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Current Epoch = " + currentEpoch.toString)
              println("Holder " + holderIndex.toString + " alpha = " + keys.alpha.toDouble+"\nEta:"+Base58.encode(eta))
            }
            inbox = Map()
          case _ =>
        }
        if (globalSlot == localSlot && updating) {
          if (maxBlockNumber > 0) {
            if (holderIndex == SharedData.printingHolder) {
              if (blocks.get(localChain.head).get.number >= maxBlockNumber) {
                val f_eff = blocks.get(localChain.head).get.number.toDouble/localChain.lastActiveSlot(globalSlot).get
                val delta_dist = localChain.slotIntervalDist
                println(s"<f> = $f_eff")
                println("Histogram:")
                delta_dist.foreach(e=> println(e._1.toString+" "+e._2.toString))
                val file = new File(inputSeedString+"_"+simLabel+"_hist.dat")
                val bw = new BufferedWriter(new FileWriter(file))
                bw.write(s"<f> = $f_eff")
                bw.newLine()
                delta_dist.foreach(e => {
                  bw.write(e._1.toString+" "+e._2.toString)
                  bw.newLine()
                })
                bw.close()
                actorStalled = true
                SharedData.killFlag = true
              }
            }
          }
          if (!useFencing) {
            forgeBlock(keys)
          }
          if (globalSlot%kesStoreInterval == (phase*kesStoreInterval).toInt) {
            val keyTime = keys.sk_kes.time(kes)
            if (keyTime < globalSlot) {
              keys.sk_kes.update_fast(kes, globalSlot)
            }
            keyFile = Some(KeyFile.update(keyFile.get,keys.sk_kes,password,keyDir,serializer,salt,derivedKey))
          }
          if (globalSlot%chainStoreInterval == (phase*chainStoreInterval).toInt) {
            chainStorage.store(localChain,dataBaseCID,serializer)
          }
        }
      }
      if (!useFencing) while (tinePoolWithPrefix.nonEmpty && updating) {
        selectTine()
      }
      if (useFencing) roundBlock match {
        case 0 =>
          forgeBlock(keys)
          roundBlock = 1
          routerRef ! Flag(selfWrapper,"updateSlot")
        case _ if chainUpdateLock =>
          if (tinePoolWithPrefix.isEmpty) {
            chainUpdateLock = false
          } else {
            if (holderIndex == SharedData.printingHolder && printFlag) {
              println("Holder " + holderIndex.toString + " Checking Tine")
            }
            selectTine()
          }
        case _ =>
      }
      if (holderIndex == SharedData.printingHolder) SharedData.numTxsMempool = memPool.keySet.size
      if (holderIndex == SharedData.printingHolder && useGui && globalSlot > 0) {
        SharedData.walletInfo =
          (wallet.getNumPending,wallet.getConfirmedTxCounter,wallet.getConfirmedBalance,wallet.getPendingBalance)
        SharedData.issueTxInfo = Some((keys.pkw,inbox))
        SharedData.selfWrapper = Some(selfWrapper)
        SharedData.blockTime = {
          val head = localChain.head
          globalSlot.toDouble/getBlockHeader(head).get._9.toDouble
        }
        SharedData.activeSlots = 1.0/SharedData.blockTime
        SharedData.txsPerSecond = {
          var net = 0
          for (entry<-localState.toSeq) {
            net += entry._2._3
          }
          net.toDouble/globalSlot.toDouble
        }
        SharedData.activePeers = holders.size
        SharedData.activeStake = {
          var out = 0.0
          for (info<-inbox) {
            val pks = info._2._2
            out += relativeStake(ByteArrayWrapper(pks._1++pks._2++pks._3),stakingState).toDouble
          }
          out += relativeStake(keys.pkw,stakingState).toDouble
          out
        }
        for (holder<-holders) {
          inbox.toList.find(info=>info._2._1 == holder) match {
            case Some(inboxInfo) => Try{
              val hpks:PublicKeys = inboxInfo._2._2
              val hpk:PublicKeyW = ByteArrayWrapper(hpks._1++hpks._2++hpks._3)
              val str = holder.actorPath.toString
              wallet.confirmedState.get(hpk) match {
                case Some(st) =>
                  val ha = relativeStake(hpk,wallet.confirmedState).toDouble
                  SharedData.confirmedBalance = SharedData.confirmedBalance + (str->st._1)
                  SharedData.confirmedAlpha = SharedData.confirmedAlpha + (str->ha)
                case None =>
              }
              stakingState.get(hpk) match {
                case Some(st) =>
                  val ha = relativeStake(hpk,stakingState).toDouble
                  SharedData.stakingBalance = SharedData.stakingBalance + (str->st._1)
                  SharedData.stakingAlpha = SharedData.stakingAlpha + (str->ha)
                case None =>
              }
            }
            case None =>
          }
        }
        Try{
          val hpk:PublicKeyW = keys.pkw
          val str = selfWrapper.actorPath.toString
          wallet.confirmedState.get(hpk) match {
            case Some(st) =>
              val ha = relativeStake(hpk,wallet.confirmedState).toDouble
              SharedData.confirmedBalance = SharedData.confirmedBalance + (str->st._1)
              SharedData.confirmedAlpha = SharedData.confirmedAlpha + (str->ha)
            case None =>
          }
          stakingState.get(hpk) match {
            case Some(st) =>
              val ha = relativeStake(hpk,stakingState).toDouble
              SharedData.stakingBalance = SharedData.stakingBalance + (str->st._1)
              SharedData.stakingAlpha = SharedData.stakingAlpha + (str->ha)
            case None =>
          }
        }
      }
      updating = false
    }
  }

}
