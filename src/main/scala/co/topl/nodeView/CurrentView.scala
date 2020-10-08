package co.topl.nodeView

case class CurrentView[HIS, MS, VL, MP](history: HIS, state: MS, vault: VL, pool: MP)
