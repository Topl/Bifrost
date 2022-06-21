package co.topl

import com.google.protobuf.ByteString
import scodec.bits.ByteVector

import scala.language.implicitConversions

package object grpc {

  implicit def byteStringToByteVector(byteString: ByteString): ByteVector =
    ByteVector(byteString.asReadOnlyByteBuffer())

  implicit def byteVectorToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toByteBuffer)
}
