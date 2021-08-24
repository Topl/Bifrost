package co.topl

import co.topl.models.utility.{Lengths, Sized}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

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
  type Epoch = Long
  type Address = TypedIdentifier
  type BoxReference = (Address, Nonce)
  type TaktikosBoxReference = (TaktikosAddress, Nonce)
  type PolyOutput = (Address, Int128)
  type ArbitOutput = (Address, Int128)
  type AssetOutput = (Address, Asset.Value)

  type TxRoot = Sized.Strict[Bytes, Lengths.`32`.type]
  type BloomFilter = Sized.Strict[Bytes, Lengths.`256`.type]

  type Account = Propositions.PublicKeyEd25519
  type Root = Propositions.PublicKeyEd25519
  type StakeAddress = Propositions.PublicKeyEd25519

  object Bytes {
    def apply(array: Array[Byte]): Bytes = new ArraySeq.ofByte(array)
  }

  @newtype case class TypedBytes(allBytes: Bytes) {
    def typePrefix: TypePrefix = allBytes.head
    def dataBytes: Bytes = allBytes.tail
  }

  object TypedBytes {

    def apply(prefix: TypePrefix, dataBytes: Bytes): TypedBytes =
      dataBytes.prepended(prefix).coerce
  }
}
