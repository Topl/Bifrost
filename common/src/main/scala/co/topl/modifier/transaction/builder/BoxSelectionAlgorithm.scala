package co.topl.modifier.transaction.builder

import co.topl.modifier.box.BoxId
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

sealed trait BoxSelectionAlgorithm

object BoxSelectionAlgorithms {
  case object All extends BoxSelectionAlgorithm
  case object SmallestFirst extends BoxSelectionAlgorithm
  case object LargestFirst extends BoxSelectionAlgorithm
  case class Specific(ids: List[BoxId]) extends BoxSelectionAlgorithm
}

object BoxSelectionAlgorithm {
  val jsonDecoder: Decoder[BoxSelectionAlgorithm] = deriveDecoder
  val jsonEncoder: Encoder[BoxSelectionAlgorithm] = deriveEncoder
}
