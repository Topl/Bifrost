package co.topl.transactiongenerator.models

import co.topl.brambl.models.LockAddress
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box.Box
import co.topl.brambl.models.box.Lock

case class Wallet(
  spendableBoxes: Map[TransactionOutputAddress, Box],
  propositions:   Map[LockAddress, Lock]
)
