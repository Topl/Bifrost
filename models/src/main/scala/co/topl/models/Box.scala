package co.topl.models

sealed abstract class Box

case class PolyBox(evidence: Evidence, nonce: Nonce, value: Int128) extends Box

case class ArbitBox(evidence: Evidence, nonce: Nonce, value: Int128) extends Box

case class AssetBox(evidence: Evidence, nonce: Nonce, value: Asset.Value) extends Box

case class RegistrationBox(registration: Registration, signature: Signature) extends Box

case class TaktikosBox(
  taktikosAddress: TaktikosAddress,
  nonce:           Nonce,
  value:           Int128,
  registration:    Registration,
  signature:       Signature
) extends Box
