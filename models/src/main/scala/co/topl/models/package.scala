package co.topl

import scala.collection.immutable.{ArraySeq, ListMap}

package object models {
  type Bytes = ArraySeq[Byte]
  type Nonce = Bytes
  type TypePrefix = Byte
  type TypedIdentifier = TypedBytes
  type Int128 = Sized.Max[BigInt, Lengths.`128`.type]
  type Timestamp = Long
  type Slot = Long
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
  type VrfCertificate = Bytes
  type KesCertificate = Bytes

  object Bytes {
    def apply(array: Array[Byte]): Bytes = new ArraySeq.ofByte(array)
  }
}
