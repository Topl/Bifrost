package co.topl.transactiongenerator.models

import co.topl.models.{Box, Proposition, TypedEvidence}

case class Wallet(
  spendableBoxes: Map[Box.Id, Box],
  propositions:   Map[TypedEvidence, Proposition]
)
