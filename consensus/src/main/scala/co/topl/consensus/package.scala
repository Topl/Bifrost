package co.topl

import co.topl.models.{BlockHeaderV2, Bytes, TypedBytes}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import scala.language.implicitConversions

package object consensus {
  type Hash = Bytes
  type SecretKey = TypedBytes
  type PublicKey = TypedBytes

  @newtype class ValidatedBlockHeader(val header: BlockHeaderV2)

  private[consensus] object ValidatedBlockHeader {

    def apply(blockHeaderV2: BlockHeaderV2): ValidatedBlockHeader =
      blockHeaderV2.coerce
  }
}
