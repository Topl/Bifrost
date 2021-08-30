package co.topl.stakeholder

import io.iohk.iodb.ByteArrayWrapper
import co.topl.stakeholder.cases.SendTx
import co.topl.stakeholder.components.{Tine, Transaction}
import co.topl.stakeholder.primitives.SharedData
import co.topl.stakeholder.primitives.Base58

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

  def updateWallet():Unit = ???

  /**
    * apply each block in chain to passed local state
    * @param ls old local state to be updated
    * @param c chain of block ids
    * @return updated localstate
    */

  def updateLocalState(ls:State, c:Tine): Option[State] = ???

  def updateLocalState(ls:State, id:SlotId):Option[State] = ???

  def trimMemPool(): Unit = ???

  /**
    * collects all transaction on the ledger of each block in the passed chain and adds them to the buffer
    * @param c chain to collect transactions
    */

  def collectLedger(c:Tine): Unit = ???

  /**
    * sorts buffer and adds transaction to ledger during block forging
    * @param pkw public key triad of forger
    * @return list of transactions
    */

  def chooseLedger(pkw:PublicKeyW,mp:MemPool,s:State): TransactionSeq = ???

}
