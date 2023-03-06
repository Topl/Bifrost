package co.topl.transactiongenerator.models

import co.topl.brambl.models.Evidence
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.box.Box
import co.topl.brambl.models.box.Lock
import quivr.models.Proposition

case class Wallet(
  spendableBoxes: Map[TransactionOutputAddress, Box],
  propositions:   Map[Evidence.Sized32, Lock]
)
