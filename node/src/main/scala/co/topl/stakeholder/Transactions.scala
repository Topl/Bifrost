package co.topl.stakeholder

import co.topl.stakeholder.components.{Serializer, Transaction}
import co.topl.stakeholder.primitives.Types.{PublicKeyW, State}
import co.topl.stakeholder.primitives.{Ratio, Sig}
import scala.math.BigInt

/**
 * AMS 2020:
 * Tx methods required by the wallet and the Stakeholder actors
 */

trait Transactions {

  def verifyTX(transaction: Transaction, sig: Sig, serializer: Serializer): Boolean = ???

  /**
   * applies an individual transaction to state
   * @param ls old local state to be updated
   * @param forger sig public key of the forger
   * @return updated localstate
   */
  def applyTransaction(t: Transaction, ls: State, forger: PublicKeyW, fee_r: Ratio): Option[State] = ???
}
