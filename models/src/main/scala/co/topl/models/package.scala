package co.topl

import co.topl.models.utility.{Lengths, Sized}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import scala.collection.immutable.{ArraySeq, ListMap}
import scala.language.implicitConversions

package object models {
  type Bytes = ArraySeq[Byte]
  type Nonce = Bytes
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
  type BoxReference = (Address, Nonce)
  type TaktikosBoxReference = (TaktikosAddress, Nonce)
  type PolyOutput = (Address, Int128)
  type ArbitOutput = (Address, Int128)
  type AssetOutput = (Address, Box.Values.Asset)
  type SlotId = (Slot, TypedIdentifier)
  type TxRoot = Sized.Strict[Bytes, Lengths.`32`.type]
  type BloomFilter = Sized.Strict[Bytes, Lengths.`256`.type]
  type Rho = Sized.Strict[Bytes, Lengths.`64`.type]
  type Account = Propositions.PublicKeyEd25519
  type Root = Propositions.PublicKeyEd25519
  type StakeAddress = Propositions.PublicKeyEd25519

  object Bytes {
    def apply(array:       Array[Byte]): Bytes = new ArraySeq.ofByte(array)
    def toByteArray(bytes: Bytes): Array[Byte] = bytes.toArray

    def concat(arrays: Bytes*): Bytes = {
      var length = 0
      for (array <- arrays)
        length += array.length
      val result = new Array[Byte](length)
      var pos = 0
      for (array <- arrays) {
        System.arraycopy(array, 0, result, pos, array.length)
        pos += array.length
      }
      Bytes(result)
    }
    def empty: Bytes = Bytes(Array())
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
