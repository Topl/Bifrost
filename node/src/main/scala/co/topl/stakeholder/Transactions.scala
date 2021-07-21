package co.topl.stakeholder

import co.topl.components.{Serializer, Transaction}
import co.topl.primitives.Types.{PublicKeyW, State}
import co.topl.primitives.{Ratio, Sig}
import scala.math.BigInt

/**
  * AMS 2020:
  * Tx methods required by the wallet and the Stakeholder actors
  */

trait Transactions {

  def verifyTX(transaction: Transaction,sig:Sig,serializer: Serializer): Boolean = {
    sig.verify(
      transaction.signature,
      transaction.receiver.data
        ++transaction.delta.toByteArray
        ++transaction.sid.data
        ++serializer.getBytes(transaction.nonce),
      transaction.sender.data.take(sig.keyLength)
    )
  }

  /**
    * applies an individual transaction to state
    * @param ls old local state to be updated
    * @param forger sig public key of the forger
    * @return updated localstate
    */
  def applyTransaction(t: Transaction,ls:State, forger:PublicKeyW, fee_r:Ratio): Option[State] = {
    var nls:State = ls
    val validSender = nls.keySet.contains(t.sender)
    val txC_s:Int = nls(t.sender)._3
    if (validSender && t.nonce == txC_s) {
      val fee:BigInt = (Ratio(t.delta)*fee_r).round
      val validRecip = nls.keySet.contains(t.receiver)
      val validFunds = nls(t.sender)._1 >= t.delta
      if (validRecip && validFunds) {
        if (t.sender == t.receiver && t.sender != forger) {
          val s_net:BigInt = nls(t.sender)._1
          val f_net:BigInt = nls(forger)._1
          val f_txC:Int = nls(forger)._3
          val s_new: BigInt = s_net - fee
          val f_new: BigInt = f_net + fee
          nls -= t.sender
          nls -= forger
          nls += (t.sender -> (s_new,true,t.nonce+1))
          nls += (forger -> (f_new,true,f_txC))
        } else if (t.sender == forger) {
          val s_net:BigInt = nls(t.sender)._1
          val r_net:BigInt = nls(t.receiver)._1
          val r_txC:Int = nls(t.receiver)._3
          val s_new: BigInt = s_net - t.delta + fee
          val r_new: BigInt = r_net + t.delta - fee
          nls -= t.sender
          nls -= t.receiver
          nls += (t.sender -> (s_new,true,t.nonce+1))
          nls += (t.receiver -> (r_new,true,r_txC))
        } else if (t.receiver == forger) {
          val s_net:BigInt = nls(t.sender)._1
          val r_net:BigInt = nls(t.receiver)._1
          val r_txC:Int = nls(t.receiver)._3
          val s_new: BigInt = s_net - t.delta
          val r_new: BigInt = r_net + t.delta
          nls -= t.sender
          nls -= t.receiver
          nls += (t.sender -> (s_new,true,t.nonce+1))
          nls += (t.receiver -> (r_new,true,r_txC))
        } else if (!nls.keySet.contains(forger)) {
          val s_net:BigInt = nls(t.sender)._1
          val r_net:BigInt = nls(t.receiver)._1
          val r_txC:Int = nls(t.receiver)._3
          val s_new: BigInt = s_net - t.delta
          val r_new: BigInt = r_net + t.delta - fee
          nls -= t.sender
          nls -= t.receiver
          nls += (t.sender -> (s_new,true,t.nonce+1))
          nls += (t.receiver -> (r_new,true,r_txC))
        } else {
          val s_net:BigInt = nls(t.sender)._1
          val r_net:BigInt = nls(t.receiver)._1
          val r_txC:Int = nls(t.receiver)._3
          val f_net:BigInt = nls(forger)._1
          val f_txC:Int = nls(forger)._3
          val s_new: BigInt = s_net - t.delta
          val r_new: BigInt = r_net + t.delta - fee
          val f_new: BigInt = f_net + fee
          nls -= t.sender
          nls -= t.receiver
          nls -= forger
          nls += (t.sender -> (s_new,true,t.nonce+1))
          nls += (t.receiver -> (r_new,true,r_txC))
          nls += (forger -> (f_new,true,f_txC))
        }
        Some(nls)
      } else if (validFunds) {
        if (t.sender == forger) {
          val s_net:BigInt = nls(t.sender)._1
          val r_net:BigInt = 0
          val s_new: BigInt = s_net - t.delta + fee
          val r_new: BigInt = r_net + t.delta - fee
          nls -= t.sender
          nls += (t.sender -> (s_new,true,t.nonce+1))
          nls += (t.receiver -> (r_new,true,0))
        } else if (!nls.keySet.contains(forger)) {
          val s_net:BigInt = nls(t.sender)._1
          val r_net:BigInt = 0
          val s_new: BigInt = s_net - t.delta
          val r_new: BigInt = r_net + t.delta - fee
          nls -= t.sender
          nls += (t.sender -> (s_new,true,t.nonce+1))
          nls += (t.receiver -> (r_new,true,0))
        } else {
          val s_net:BigInt = nls(t.sender)._1
          val r_net:BigInt = 0
          val f_net:BigInt = nls(forger)._1
          val f_txC = nls(forger)._3
          val s_new: BigInt = s_net - t.delta
          val r_new: BigInt = r_net + t.delta - fee
          val f_new: BigInt = f_net + fee
          nls -= t.sender
          nls -= forger
          nls += (t.sender -> (s_new,true,t.nonce+1))
          nls += (t.receiver -> (r_new,true,0))
          nls += (forger -> (f_new,true,f_txC))
        }
        Some(nls)
      } else {
        None
      }
    } else {
      None
    }
  }
}
