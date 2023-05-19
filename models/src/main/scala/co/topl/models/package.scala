package co.topl

import co.topl.models.utility.Lengths
import co.topl.models.utility.Sized
import com.google.protobuf.ByteString
import io.estatico.newtype.macros.newsubtype
import io.estatico.newtype.macros.newtype

import scala.language.implicitConversions

package object models {

  type Bytes = ByteString
  type Eta = Sized.Strict[Bytes, Eta.Length]

  object Eta {
    type Length = Lengths.`32`.type
  }

  type TypePrefix = Byte
  type Timestamp = Long
  type Slot = Long
  type Epoch = Long

  @newsubtype case class NetworkPrefix(value: Int)

  type TxRoot = Sized.Strict[Bytes, Lengths.`32`.type]
  type BloomFilter = Sized.Strict[Bytes, Lengths.`256`.type]
  @newtype case class Rho(sizedBytes: Sized.Strict[Bytes, Lengths.`64`.type])
  @newtype case class RhoTestHash(sizedBytes: Sized.Strict[Bytes, Lengths.`64`.type])
  @newtype case class RhoNonceHash(sizedBytes: Sized.Strict[Bytes, Lengths.`64`.type])
  type Digest32 = Sized.Strict[Bytes, Lengths.`32`.type]

}
