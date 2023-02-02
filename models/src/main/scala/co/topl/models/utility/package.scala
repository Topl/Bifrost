package co.topl.models

import com.google.protobuf.ByteString
import scodec.bits.ByteVector

import scala.language.implicitConversions

package object utility extends Isomorphism.Ops with BifrostMorphismInstances {

  implicit def byteStringToByteVector(byteString: ByteString): ByteVector =
    ByteVector(byteString.asReadOnlyByteBuffer())

  implicit def byteVectorToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toByteBuffer)

  implicit def blockIdAsTypedBytes(blockId: co.topl.consensus.models.BlockId): TypedIdentifier =
    TypedBytes(IdentifierTypes.Block.HeaderV2, blockId.value)

  implicit def ioTx32IdAsTypedBytes(id: co.topl.brambl.models.Identifier.IoTransaction32): TypedIdentifier =
    TypedBytes(IdentifierTypes.Transaction, id.evidence.get.digest.get.value)
}
