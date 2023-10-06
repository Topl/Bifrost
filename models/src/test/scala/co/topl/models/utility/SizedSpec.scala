package co.topl.models.utility

import cats.effect.IO
import cats.implicits._
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.Lengths._
import com.google.protobuf.ByteString
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class SizedSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  type F[A] = IO[A]

  test("enforce a strict size limit on correctly sized data") {
    val data = ByteString.copyFrom(Array.fill[Byte](4)(0))

    Sized
      .strict[ByteString, Lengths.`4`.type](data)
      .map(_.data)
      .leftMap(e => new IllegalArgumentException(e.toString))
      .pure[F]
      .rethrow
      .assertEquals(data)
  }

  test("enforce a max size limit on correctly sized data") {
    val data = ByteString.copyFrom(Array.fill[Byte](3)(0))

    Sized
      .max[ByteString, Lengths.`4`.type](data)
      .map(_.data)
      .leftMap(e => new IllegalArgumentException(e.toString))
      .pure[F]
      .rethrow
      .assertEquals(data)
  }

  test("reject strict incorrectly sized data") {
    val data = ByteString.copyFrom(Array.fill[Byte](5)(0))

    Sized
      .strict[ByteString, Lengths.`4`.type](data)
      .as(new IllegalArgumentException("Unexpected success"))
      .swap
      .pure[F]
      .rethrow
      .assertEquals(Sized.InvalidLength(5))
  }

  test("reject max incorrectly sized data") {
    val data = ByteString.copyFrom(Array.fill[Byte](5)(0))

    Sized
      .max[ByteString, Lengths.`4`.type](data)
      .as(new IllegalArgumentException("Unexpected success"))
      .swap
      .pure[F]
      .rethrow
      .assertEquals(Sized.InvalidLength(5))
  }

  test("accept correctly sized Int128") {
    val bigInt = BigInt(Int.MaxValue)
    Sized
      .max[BigInt, Lengths.`64`.type](bigInt)
      .map(_.data)
      .leftMap(e => new IllegalArgumentException(e.toString))
      .pure[F]
      .rethrow
      .assertEquals(bigInt)
  }

  test("reject incorrectly sized Int128") {
    val bigInt = BigInt(Long.MaxValue)
    Sized
      .max[BigInt, Lengths.`32`.type](bigInt)
      .as(new IllegalArgumentException("Unexpected success"))
      .swap
      .pure[F]
      .rethrow
      .assertEquals(Sized.InvalidLength(bigInt.bitLength))
  }

}
