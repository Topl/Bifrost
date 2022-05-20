package co.topl

import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Sized}
import co.topl.models.utility.HasLength.instances._
import io.estatico.newtype.macros.{newsubtype, newtype}
import io.estatico.newtype.ops._
import scodec.bits.ByteVector

import scala.language.implicitConversions

package object models {
  type Bytes = ByteVector
  val Bytes = ByteVector
  type BoxNonce = Long
  type Eta = Sized.Strict[Bytes, Lengths.`32`.type]
  type Evidence = Sized.Strict[Bytes, Lengths.`32`.type]

  case class TypedEvidence(typePrefix: TypePrefix, evidence: Evidence) {
    def allBytes: Bytes = typePrefix +: evidence.data
  }

  object TypedEvidence {
    val empty: TypedEvidence = TypedEvidence(0: Byte, Sized.strictUnsafe(Bytes(Array.fill(32)(0: Byte))))
  }

  type TypePrefix = Byte
  type TypedIdentifier = TypedBytes
  type Int128 = Sized.Max[BigInt, Lengths.`128`.type]
  type Timestamp = Long
  type Slot = Long
  type Epoch = Long

  @newsubtype case class NetworkPrefix(value: Byte)

  type TxRoot = Sized.Strict[Bytes, Lengths.`32`.type]
  type BloomFilter = Sized.Strict[Bytes, Lengths.`256`.type]
  @newtype case class Rho(sizedBytes: Sized.Strict[Bytes, Lengths.`64`.type])
  @newtype case class RhoTestHash(sizedBytes: Sized.Strict[Bytes, Lengths.`64`.type])
  @newtype case class RhoNonceHash(sizedBytes: Sized.Strict[Bytes, Lengths.`64`.type])
  type StakeAddress = Propositions.Knowledge.Ed25519
  type Digest32 = Sized.Strict[Bytes, Lengths.`32`.type]
  type TransactionData = Sized.Max[Latin1Data, Lengths.`127`.type]

  case class SlotId(slot: Slot, blockId: TypedIdentifier)

  type BlockBodyV2 = List[TypedIdentifier]

  @newtype case class TypedBytes(allBytes: Bytes) {
    def typePrefix: TypePrefix = allBytes.head
    def dataBytes: Bytes = allBytes.tail
  }

  object TypedBytes {

    def apply(prefix: TypePrefix, dataBytes: Bytes): TypedBytes =
      (prefix +: dataBytes).coerce
  }
}
