package co.topl.transactiongenerator.models

case class Wallet(
  spendableBoxes: Map[Box.Id, Box],
  propositions:   Map[Evidence, Proposition]
)
