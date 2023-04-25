package co.topl.models

import com.google.protobuf.ByteString
import scodec.bits.ByteVector

import scala.language.implicitConversions

package object utility {

  implicit def byteStringToByteVector(byteString: ByteString): ByteVector =
    ByteVector(byteString.asReadOnlyByteBuffer())

  implicit def byteVectorToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toByteBuffer)

  implicit def byteStringToByteArray(byteString: ByteString): Array[Byte] = byteString.toByteArray

  implicit def byteArrayToByteString(byteArray: Array[Byte]): ByteString = ByteString.copyFrom(byteArray)

}
