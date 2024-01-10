package co.topl.consensus.models

sealed trait ChainSelectionOutcome {
  def isX: Boolean
  def isY: Boolean
}

object ChainSelectionOutcome {

  sealed trait XOutcome {
    self: ChainSelectionOutcome =>
    override def isX: Boolean = true
    override def isY: Boolean = false
  }

  sealed trait YOutcome {
    self: ChainSelectionOutcome =>
    override def isX: Boolean = false
    override def isY: Boolean = true
  }
  case object XStandard extends ChainSelectionOutcome with XOutcome
  case object YStandard extends ChainSelectionOutcome with YOutcome
  case object XDensity extends ChainSelectionOutcome with XOutcome
  case object YDensity extends ChainSelectionOutcome with YOutcome
}
