package co.topl.transactiongenerator.models

import co.topl.models.{Box, Proposition, TypedEvidence}

case class Wallet(
  spendableBoxIds: Map[Box.Id, Box],
  propositions:    Map[TypedEvidence, Proposition]
)
