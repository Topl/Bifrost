package co.topl

import scala.collection.immutable.{ArraySeq, ListMap}

package object models {
  type Nonce = Long
  type Bytes = ArraySeq.ofByte
  type TypePrefix = Byte
  type TypedIdentifier = (Byte, Bytes)
  type Int128 = Sized.Max[BigInt, Lengths.`128`.type]
  type Timestamp = Long
  type Attestation = ListMap[Proposition, Proof]
  type Registration = Bytes
  type Signature = Bytes
  type Evidence = Bytes
  type Address = TypedIdentifier
  type BoxReference = (Address, Nonce)
  type TaktikosBoxReference = (TaktikosAddress, Nonce)
  type PolyOutput = (Address, Int128)
  type ArbitOutput = (Address, Int128)
  type AssetOutput = (Address, Asset.Value)
}
