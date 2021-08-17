package co.topl.models

import co.topl.models.StringDataTypes.Latin1Data

case class PolyBox(evidence: Evidence, nonce: Nonce, value: Int128)
case class ArbitBox(evidence: Evidence, nonce: Nonce, value: Int128)
case class AssetBox(evidence: Evidence, nonce: Nonce, value: AssetValue)
case class RegistrationBox(registration: Registration, signature: Signature)

case class TaktikosBox(
  taktikosAddress: TaktikosAddress,
  nonce:           Nonce,
  value:           Int128,
  registration:    Registration,
  signature:       Signature
)

case class AssetValue(
  quantity:     Int128,
  assetCode:    AssetCode,
  securityRoot: Bytes,
  metadata:     Option[Sized.Max[Latin1Data, Lengths.`127`.type]]
)
case class AssetCode(version: Byte, issuer: Address, shortName: String)
