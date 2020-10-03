package co.topl.nodeView

case class CurrentView[HIS, MS, MP](history: HIS, state: MS, pool: MP)
