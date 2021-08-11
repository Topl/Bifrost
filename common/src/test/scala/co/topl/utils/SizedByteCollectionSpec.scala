package co.topl.utils

import co.topl.utils.SizedBytes.InvalidSize
import co.topl.utils.SizedBytes.Types.{ByteVector128, ByteVector28, ByteVector32, ByteVector4}
import org.scalatest.flatspec.AnyFlatSpec
import co.topl.utils.SizedBytes.implicits._
import org.scalacheck.Prop.forAll
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scodec.bits.ByteVector

class SizedByteCollectionSpec extends AnyFlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  def testCollection[T: SizedBytes](collectionName: String, expectedSize: Int): Unit =
    collectionName should s"validate byte arrays if size is $expectedSize" in {
      forAll { (bytes: Array[Byte]) =>
        if (bytes.length == expectedSize) SizedBytes[T].validated(bytes).isRight shouldBe true
        else SizedBytes[T].validated(bytes).isLeft shouldBe true
      }
    }

  testCollection[ByteVector4]("ByteVector4", 4)
  testCollection[ByteVector28]("ByteVector28", 28)
  testCollection[ByteVector32]("ByteVector32", 32)
  testCollection[ByteVector128]("ByteVector128", 128)
}
