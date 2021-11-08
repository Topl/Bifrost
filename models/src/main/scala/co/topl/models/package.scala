package co.topl

import co.topl.models.utility.{Lengths, Sized}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._
import scodec.bits.ByteVector

import scala.collection.immutable.ListMap
import scala.language.implicitConversions

package object models {
  type Bytes = ByteVector
  type BoxNonce = Long
  type Eta = Sized.Strict[Bytes, Lengths.`32`.type]
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
  type DionAddress = TypedIdentifier
  type BoxReference = (DionAddress, Eta)
  type TaktikosBoxReference = (TaktikosAddress, Eta)
  type PolyOutput = (DionAddress, Int128)
  type ArbitOutput = (DionAddress, Int128)
  type AssetOutput = (DionAddress, Box.Values.Asset)
  type SlotId = (Slot, TypedIdentifier)
  type TxRoot = Sized.Strict[Bytes, Lengths.`32`.type]
  type BloomFilter = Sized.Strict[Bytes, Lengths.`256`.type]
  type Rho = Sized.Strict[Bytes, Lengths.`64`.type]
  type Account = Propositions.Knowledge.Ed25519
  type Root = Propositions.Knowledge.Ed25519
  type StakeAddress = Propositions.Knowledge.Ed25519
  type Digest32 = Sized.Strict[Bytes, Lengths.`32`.type]

  val Bytes = ByteVector

  @newtype case class TypedBytes(allBytes: Bytes) {
    def typePrefix: TypePrefix = allBytes.head
    def dataBytes: Bytes = allBytes.tail
  }

  object TypedBytes {

    def apply(prefix: TypePrefix, dataBytes: Bytes): TypedBytes =
      (prefix +: dataBytes).coerce
  }
}
