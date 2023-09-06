package co.topl.byzantine

import co.topl.brambl.models.{LockAddress, TransactionOutputAddress}
import co.topl.brambl.models.box.{Box, Lock}

/**
 * Package transactions try to group methods and variables used on byzantine-it.TransactionTest
 */
package object transactions {

  case class Wallet(spendableBoxes: Map[TransactionOutputAddress, Box], propositions: Map[LockAddress, Lock])
}
