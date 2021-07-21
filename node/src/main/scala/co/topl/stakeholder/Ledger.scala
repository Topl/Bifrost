package co.topl.stakeholder

import io.iohk.iodb.ByteArrayWrapper
import co.topl.cases.SendTx
import co.topl.components.{Tine, Transaction}
import co.topl.primitives.SharedData
import co.topl.primitives.Base58

import scala.collection.immutable.ListMap
import scala.math.BigInt
import scala.util.{Failure, Try}
import scala.util.control.Breaks.{break, breakable}

/**
  * AMS 2020:
  * Methods regarding leger updates and state transitions using the account based transaction model,
  * Many more Tx models are to be incorporated, namely UTXOs,
  * this outlines where state updates will be executed in the system
  */

trait Ledger extends Members {

  def updateWallet():Unit = Try{
    var id = localChain.getLastActiveSlot(globalSlot).get
    val bn:Int = getBlockHeader(id).get._9
    if (bn == 0) {
      wallet.update(history.get(id).get._1)
      if (holderIndex == SharedData.printingHolder) {
        SharedData.walletInfo = (
          wallet.getNumPending,
          wallet.getConfirmedTxCounter,
          wallet.getConfirmedBalance,
          wallet.getPendingBalance
        )
        SharedData.issueTxInfo = Some((keys.pkw,inbox))
        SharedData.selfWrapper = Some(selfWrapper)
      }
    } else {
      breakable{
        while (true) {
          id = getParentId(id).get
          getBlockHeader(id) match {
            case Some(b:BlockHeader) =>
              val bni = b._9
              if (bni <= bn-confirmationDepth || bni == 0) {
                wallet.update(history.get(id).get._1)
                if (holderIndex == SharedData.printingHolder) {
                  SharedData.walletInfo = (
                    wallet.getNumPending,
                    wallet.getConfirmedTxCounter,
                    wallet.getConfirmedBalance,
                    wallet.getPendingBalance
                  )
                  SharedData.issueTxInfo = Some((keys.pkw,inbox))
                  SharedData.selfWrapper = Some(selfWrapper)
                }
                break
              }
            case None =>
              println("Error: invalid id in wallet")
              break
          }
        }
      }
    }
    for (trans:Transaction <- wallet.getPending(localState)) {
      if (!memPool.keySet.contains(trans.sid)) memPool += (trans.sid->(trans,0))
      send(selfWrapper,gossipSet(selfWrapper,holders), SendTx(trans,selfWrapper))
    }

    def collectStake():Unit = Try{
      for (entry<-wallet.confirmedState) if (!wallet.reallocated.keySet.contains(entry._1)) {
        if (wallet.isSameLedgerId(entry._1) && entry._2._1 > 0) {
          wallet.issueTx(entry._1,wallet.pkw,entry._2._1,keys.sk_sig,sig,rng,serializer) match {
            case Some(trans:Transaction) =>
              if (holderIndex == SharedData.printingHolder && printFlag)
                println("Holder " + holderIndex.toString + " Reallocated Stake")
              txCounter += 1
              memPool += (trans.sid->(trans,0))
              send(selfWrapper,gossipSet(selfWrapper,holders), SendTx(trans,selfWrapper))
              wallet.reallocated += (entry._1->trans.nonce)
            case _ =>
          }
        }
      }
    }
    collectStake()
    walletStorage.store(wallet,serializer)
  } match {
    case Failure(exception) =>
      exception.printStackTrace()
    case _ =>
  }

  /**
    * apply each block in chain to passed local state
    * @param ls old local state to be updated
    * @param c chain of block ids
    * @return updated localstate
    */

  def updateLocalState(ls:State, c:Tine): Option[State] = {
    var nls:State = ls
    var isValid = true
    for (id <- c.ordered) {
      if (isValid) getBlockHeader(id) match {
        case Some(b:BlockHeader) =>
          val (_,_,slot:Slot,_,_,_,_,pk_kes:PublicKey,_,_) = b
          val cert:Cert = b._4
          val (pk_vrf,_,_,pk_sig,_,_) = cert
          val pk_f:PublicKeyW = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
          var validForger = true
          if (slot == 0) {
            val genesisSet:GenesisSeq = blocks.get(id).get.genesisSet.get
            if (genesisSet.isEmpty) isValid = false
            if (isValid) for (entry <- genesisSet) {
              if (ByteArrayWrapper(entry._1) == genesisBytes) {
                val delta = entry._3
                val netStake:BigInt = 0
                val newStake:BigInt = netStake + delta
                val pk_g:PublicKeyW = entry._2
                if(nls.keySet.contains(pk_g)) {
                  isValid = false
                  nls -= pk_g
                }
                nls += (pk_g -> (newStake,true,0))
              }
            }
          } else {
            //apply forger reward
            if (nls.keySet.contains(pk_f)) {
              val netStake: BigInt = nls(pk_f)._1
              val txC:Int = nls(pk_f)._3
              val newStake: BigInt = netStake + forgerReward
              nls -= pk_f
              nls += (pk_f -> (newStake,true,txC))
            } else {
              validForger = false
            }
            //apply transactions
            if (validForger) {
              for (trans <- blocks.get(id).get.blockBody.get) {
                if (verifyTransaction(trans)) {
                  applyTransaction(trans, nls, pk_f, fee_r) match {
                    case Some(value: State) =>
                      nls = value
                    case _ => isValid = false
                  }
                } else {
                  isValid = false
                }
              }
            } else {
              isValid = false
            }
          }
        case _ =>
      }
      if (!isValid) {
        println(s"Holder $holderIndex ledger error on slot "+id._1+" block id:"+Base58.encode(id._2.data))
        SharedData.throwError(holderIndex)
      }
    }
    if (isValid) {
      Some(nls)
    } else {
      None
    }
  }

  def updateLocalState(ls:State, id:SlotId):Option[State] = {
    var nls:State = ls
    var isValid = true
    if (isValid) getBlockHeader(id) match {
      case Some(b:BlockHeader) =>
        val (_,_,slot:Slot,_,_,_,_,pk_kes:PublicKey,_,_) = b
        val cert:Cert = b._4
        val (pk_vrf,_,_,pk_sig,_,_) = cert
        val pk_f:PublicKeyW = ByteArrayWrapper(pk_sig++pk_vrf++pk_kes)
        var validForger = true
        if (slot == 0) {
          val genesisSet:GenesisSeq = blocks.get(id).get.genesisSet.get
          if (genesisSet.isEmpty) isValid = false
          if (isValid) for (entry <- genesisSet) {
            if (ByteArrayWrapper(entry._1) == genesisBytes) {
              val delta = entry._3
              val netStake:BigInt = 0
              val newStake:BigInt = netStake + delta
              val pk_g:PublicKeyW = entry._2
              if(nls.keySet.contains(pk_g)) {
                isValid = false
                nls -= pk_g
              }
              nls += (pk_g -> (newStake,true,0))
            }
          }
        } else {
          //apply forger reward
          if (nls.keySet.contains(pk_f)) {
            val netStake: BigInt = nls(pk_f)._1
            val txC:Int = nls(pk_f)._3
            val newStake: BigInt = netStake + forgerReward
            nls -= pk_f
            nls += (pk_f -> (newStake,true,txC))
          } else {
            validForger = false
          }
          //apply transactions
          if (validForger) {
            for (trans <- blocks.get(id).get.blockBody.get) {
              if (verifyTransaction(trans)) {
                applyTransaction(trans, nls, pk_f, fee_r) match {
                  case Some(value:State) =>
                    nls = value
                  case _ => isValid = false
                }
              } else {
                isValid = false
              }
            }
          } else {
            isValid = false
          }
        }
      case _ =>
    }
    if (!isValid) {
      println(s"Holder $holderIndex ledger error on slot "+id._1+" block id:"+Base58.encode(id._2.data))
      SharedData.throwError(holderIndex)
    }
    if (isValid) {
      Some(nls)
    } else {
      None
    }
  }

  def trimMemPool(): Unit = {
    val mp = memPool
    for (entry <- mp) {
      if (entry._2._2 < confirmationDepth) {
        val cnt = entry._2._2 + 1
        memPool -= entry._1
        memPool += (entry._1 -> (entry._2._1,cnt))
      } else {
        memPool -= entry._1
      }
      if (entry._2._1.nonce < localState(entry._2._1.sender)._3) {
        memPool -= entry._1
      }
    }
  }

  /**
    * collects all transaction on the ledger of each block in the passed chain and adds them to the buffer
    * @param c chain to collect transactions
    */

  def collectLedger(c:Tine): Unit = {
    for (id <- c.ordered) {
      for (trans <- blocks.get(id).get.blockBody.get) {
        if (!memPool.keySet.contains(trans.sid)) {
          if (verifyTransaction(trans)) memPool += (trans.sid->(trans,0))
        }
      }
    }
  }

  /**
    * sorts buffer and adds transaction to ledger during block forging
    * @param pkw public key triad of forger
    * @return list of transactions
    */

  def chooseLedger(pkw:PublicKeyW,mp:MemPool,s:State): TransactionSeq = {
    var ledger: List[Transaction] = List()
    var ls: State = s
    val sortedBuffer = ListMap(mp.toSeq.sortWith(_._2._1.nonce < _._2._1.nonce): _*)
    breakable {
      for (entry <- sortedBuffer) {
        val transaction:Transaction = entry._2._1
        val transactionCount:Int = transaction.nonce
        if (transactionCount == ls(transaction.sender)._3 && verifyTransaction(transaction)) {
          applyTransaction(transaction,ls, pkw,fee_r) match {
            case Some(value:State) =>
              ledger ::= entry._2._1
              ls = value
            case _ =>
          }
          if (ledger.length >= txPerBlock) break
        }
      }
    }
    ledger.reverse
  }

}
