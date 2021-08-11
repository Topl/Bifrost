package co.topl.utils.codecs

import cats.implicits._
import co.topl.utils.SizedBytes
import co.topl.utils.SizedBytes.InvalidSize
import co.topl.utils.SizedBytes.Types._
import co.topl.utils.SizedBytes.implicits._
import scodec.bits.ByteVector

object SizedByteCollectionCodec {

  trait AsBytesInstances {
    implicit val byteVectorAsBytes: AsBytes[Infallible, ByteVector] = AsBytes.infallible(_.toArray)
    implicit val byteVector128AsBytes: AsBytes[Infallible, ByteVector128] = AsBytes.infallible(_.toArray)
    implicit val byteVector64AsBytes: AsBytes[Infallible, ByteVector64] = AsBytes.infallible(_.toArray)
    implicit val byteVector32AsBytes: AsBytes[Infallible, ByteVector32] = AsBytes.infallible(_.toArray)
    implicit val byteVector28AsBytes: AsBytes[Infallible, ByteVector28] = AsBytes.infallible(_.toArray)
    implicit val byteVector4AsBytes: AsBytes[Infallible, ByteVector4] = AsBytes.infallible(_.toArray)
  }

  trait FromBytesInstances {

    implicit val byteVectorFromBytes: FromBytes[Infallible, ByteVector] =
      FromBytes.infallible(ByteVector(_))

    implicit val byteVector128FromBytes: FromBytes[InvalidSize, ByteVector128] =
      bytes => SizedBytes[ByteVector128].validated(bytes).toValidatedNec

    implicit val byteVector64FromBytes: FromBytes[InvalidSize, ByteVector64] =
      bytes => SizedBytes[ByteVector64].validated(bytes).toValidatedNec

    implicit val byteVector32FromBytes: FromBytes[InvalidSize, ByteVector32] =
      bytes => SizedBytes[ByteVector32].validated(bytes).toValidatedNec

    implicit val byteVector28FromBytes: FromBytes[InvalidSize, ByteVector28] =
      bytes => SizedBytes[ByteVector28].validated(bytes).toValidatedNec

    implicit val byteVector4FromBytes: FromBytes[InvalidSize, ByteVector4] =
      bytes => SizedBytes[ByteVector4].validated(bytes).toValidatedNec
  }

  trait Instances extends AsBytesInstances with FromBytesInstances

  object implicits extends Instances
}
