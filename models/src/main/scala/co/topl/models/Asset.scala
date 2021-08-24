package co.topl.models

import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.utility.StringDataTypes.Latin1Data

object Asset {

  case class Value(
    quantity:     Int128,
    assetCode:    Code,
    securityRoot: Bytes,
    metadata:     Option[Sized.Max[Latin1Data, Lengths.`127`.type]]
  )
  case class Code(version: Byte, issuer: Address, shortName: String)
}
