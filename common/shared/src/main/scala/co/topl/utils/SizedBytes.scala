package co.topl.utils

import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._
import scodec.bits.{ByteOrdering, ByteVector}
import simulacrum.typeclass

import java.nio.{ByteBuffer, ByteOrder}
import scala.language.implicitConversions

/**
 * Type-class for working with a collection of bytes with a specific size.
 * @tparam T the underlying type of the collection
 */
@typeclass
trait SizedBytes[T] {

  /**
   * The size of the byte vector
   */
  def size: Int

  /**
   * Validates that the given `ByteVector` is of the expected size.
   * @param vector the `ByteVector` to validate and wrap as a `SizedByteCollection` of type `T`
   * @return the validated `T` if the vector is of the specified size or an `InvalidSize` error
   */
  def validated(vector: ByteVector): Either[SizedBytes.InvalidSize, T]

  /**
   * Validates that the given `Array[Byte]` is of the expected size.
   * @param array the `Array[Byte]` to validate and transform into collection `T`
   * @return the validated `T` if the array is of the specified size or an `InvalidSize` error
   */
  def validated(array: Array[Byte]): Either[SizedBytes.InvalidSize, T] = validated(ByteVector(array))

  /**
   * Fits the given `ByteVector` into a `T` with the expected size.
   * Truncates or pads as necessary defined by the `ByteOrdering`.
   * @param from the `ByteVector` to 'fit' into the expected size
   * @param ordering the ordering of the bytes within the `ByteVector`
   * @return a `T` containing the expected number of bytes
   */
  def fit(from: ByteVector, ordering: ByteOrdering): T

  /**
   * Fits the given `Array[Byte]` into a `T` with the expected size.
   * Truncates or pads as necessary defined by the `ByteOrdering`.
   * @param from the `Array[Byte]` to 'fit' into the expected size
   * @param ordering the ordering of the bytes within the `ByteVector`
   * @return a `T` containing the expected number of bytes
   */
  def fit(from: Array[Byte], ordering: ByteOrdering): T = fit(ByteVector(from), ordering)

  /**
   * Fits the given `ByteBuffer` into a `T` with the expected size.
   * Truncates or pads as necessary defined by the ordering of the buffer.
   * @param from the `ByteBuffer` to 'fit' into the expected size
   * @param ordering the ordering of the bytes within the `ByteVector`
   * @return a `T` containing the expected number of bytes
   */
  def fit(from: ByteBuffer): T =
    if (from.order() == ByteOrder.LITTLE_ENDIAN) fit(from.array(), ByteOrdering.LittleEndian)
    else fit(from.array(), ByteOrdering.BigEndian)

  /**
   * Transforms the value of type `T` into a `ByteVector`.
   * @param t the value to transform
   * @return a `ByteVector` containing the bytes from collection `T`
   */
  def toVector(t: T): ByteVector

  /**
   * Transforms the value of type `T` into a `Array[Byte]`.
   * @param t the value to transform
   * @return a `Array[Byte]` containing the bytes from collection `T`
   */
  def toArray(t: T): Array[Byte] = toVector(t).toArray
}

object SizedBytes {

  /**
   * Represents the error when a given byte collection is the wrong size.
   */
  case class InvalidSize()

  /**
   * Creates a new instance of `SizedByteCollection` for type `T` from a specific size,
   * construction function, and function to transform into a `ByteVector`.
   * @param collectionSize the expected size of the byte collection type
   * @param constructorFunc the collection type constructor
   * @param toVectorFunc a transformation function to convert a type `T` into a `ByteVector`
   * @tparam T the type of the byte collection
   * @return an instance of the `SizedByteCollection` type-class
   */
  def instance[T](
    collectionSize:  Int,
    constructorFunc: ByteVector => T,
    toVectorFunc:    T => ByteVector
  ): SizedBytes[T] =
    new SizedBytes[T] {
      override val size: Int = collectionSize

      override def validated(vector: ByteVector): Either[InvalidSize, T] =
        if (vector.size == collectionSize) Right(constructorFunc(vector)) else Left(InvalidSize())

      override def fit(from: ByteVector, ordering: ByteOrdering): T = (from, ordering) match {

        // if bytes vector is little endian and smaller than required, then add 0s to the right side of the vector
        case (x, ByteOrdering.LittleEndian) if x.size < collectionSize => constructorFunc(from.padRight(collectionSize))

        // if bytes vector is big endian and smaller than required, then add 0s to the left side of the vector
        case (x, ByteOrdering.BigEndian) if x.size < collectionSize => constructorFunc(from.padLeft(collectionSize))

        // if bytes vector is little endian and bigger than required, remove the most significant bytes from
        // the left side of the vector
        case (x, ByteOrdering.LittleEndian) if x.size > collectionSize => constructorFunc(from.take(collectionSize))

        // if bytes vector is big endian and bigger than required, remove the most significant bytes from
        // the right side of the vector
        case (x, ByteOrdering.BigEndian) if x.size > collectionSize => constructorFunc(from.takeRight(collectionSize))

        case (x, _) => constructorFunc(x)
      }

      override def toVector(t: T): ByteVector = toVectorFunc(t)
    }

  /**
   * Common sized-byte-collection types with instances of the `SizedByteCollection` type-class.
   */
  object Types {

    /**
     * A `ByteVector` with a size of 128.
     * @param value the underlying `ByteVector` value
     */
    @newtype
    class ByteVector128(val value: ByteVector)

    object ByteVector128 {
      val size: Int = 128

      /**
       * An instance of the `SizedBytes` type-class for `ByteVector128`
       */
      val sizedBytes: SizedBytes[ByteVector128] =
        SizedBytes.instance(size, _.coerce, _.value)
    }

    @newtype
    class ByteVector96(val value: ByteVector)

    object ByteVector96 {
      val size: Int = 96

      /**
       * An instance of the `SizedBytes` type-class for `ByteVector96`
       */
      val sizedBytes: SizedBytes[ByteVector96] =
        SizedBytes.instance(size, _.coerce, _.value)
    }

    /**
     * A `ByteVector` with a size of 64.
     * @param value the underlying `ByteVector` value
     */
    @newtype
    class ByteVector64(val value: ByteVector)

    object ByteVector64 {
      val size: Int = 64

      /**
       * An instance of the `SizedBytes` type-class for `ByteVector64`
       */
      val sizedBytes: SizedBytes[ByteVector64] = SizedBytes.instance(size, _.coerce, _.value)
    }

    /**
     * A `ByteVector` with a size of 32.
     * @param value the underlying `ByteVector` value
     */
    @newtype
    class ByteVector32(val value: ByteVector)

    object ByteVector32 {
      val size: Int = 32

      /**
       * An instance of the `SizedBytes` type-class for `ByteVector32`
       */
      val sizedBytes: SizedBytes[ByteVector32] = SizedBytes.instance(size, _.coerce, _.value)
    }

    /**
     * A `ByteVector` with a size of 28.
     * @param value the underlying `ByteVector` value
     */
    @newtype
    class ByteVector28(val value: ByteVector)

    object ByteVector28 {
      val size: Int = 28

      /**
       * An instance of the `SizedBytes` type-class for `ByteVector28`
       */
      val sizedBytes: SizedBytes[ByteVector28] = SizedBytes.instance(size, _.coerce, _.value)
    }

    /**
     * A `ByteVector` with a size of 4.
     * @param value the underlying `ByteVector` value
     */
    @newtype
    class ByteVector4(val value: ByteVector)

    /**
     * An instance of the `SizedBytes` type-class for `ByteVector4`
     */
    object ByteVector4 {
      val size: Int = 4

      val sizedBytes: SizedBytes[ByteVector4] = SizedBytes.instance(size, _.coerce, _.value)
    }
  }

  trait Instances {
    import Types._

    implicit val byteVector128: SizedBytes[ByteVector128] = ByteVector128.sizedBytes
    implicit val byteVector96: SizedBytes[ByteVector96] = ByteVector96.sizedBytes
    implicit val byteVector64: SizedBytes[ByteVector64] = ByteVector64.sizedBytes
    implicit val byteVector32: SizedBytes[ByteVector32] = ByteVector32.sizedBytes
    implicit val byteVector28: SizedBytes[ByteVector28] = ByteVector28.sizedBytes
    implicit val byteVector4: SizedBytes[ByteVector4] = ByteVector4.sizedBytes
  }

  trait Implicits extends Instances with SizedBytes.ToSizedBytesOps

  object implicits extends Implicits
}
