package co.topl

import co.topl.models.utility.{Lengths, Sized}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import scala.collection.immutable.{ArraySeq, ListMap}
import scala.language.implicitConversions

package object models {
  type Bytes = ArraySeq[Byte]
  type BoxNonce = Long
  // Epoch Nonce, 32 Bytes
  type Eta = Bytes
  type Evidence = Sized.Strict[TypedBytes, Lengths.`33`.type]
  type TypePrefix = Byte
  type TypedIdentifier = TypedBytes
  type Int128 = Sized.Max[BigInt, Lengths.`128`.type]
  type Timestamp = Long
  type Slot = Long
  type Attestation = ListMap[Proposition, Proof]
  type Registration = Bytes
  type Signature = Bytes
  type Epoch = Long
  type Address = TypedIdentifier
  type BoxReference = (Address, Eta)
  type TaktikosBoxReference = (TaktikosAddress, Eta)
  type PolyOutput = (Address, Int128)
  type ArbitOutput = (Address, Int128)
  type AssetOutput = (Address, Box.Values.Asset)

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
