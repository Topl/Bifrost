package co.topl.crypto.hash

import co.topl.crypto.utils.Hex
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait HashSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  val emptyBytes: Array[Byte] = Array.empty

  def hashCheckString[H, D: Digest](external: List[(String, String)])(implicit hash: Hash[H, D]): Unit =
    hashCheck(external.map(x => x._1.getBytes("UTF-8") -> x._2))

  def hashCheck[H, D: Digest](external: List[(Array[Byte], String)])(implicit hash: Hash[H, D]): Unit = {

    property(s"${hash.getClass.getSimpleName} size of hash should be DigestSize") {
      forAll { data: Array[Byte] =>
        Digest[D].bytes(hash.hash(data)).length shouldBe Digest[D].size
      }
    }

    property(s"${hash.getClass.getSimpleName} no collisions") {
      forAll { (x: Array[Byte], y: Array[Byte]) =>
        whenever(!x.sameElements(y)) {
          hash.hash(x) should not equal hash.hash(y)
        }
      }
    }

    property(s"${hash.getClass.getSimpleName} comparing with externally computed value") {
      external.foreach { m =>
        Hex.encode(Digest[D].bytes(hash.hash(m._1))) shouldBe m._2.toLowerCase
      }
    }

    property(s"${hash.getClass.getSimpleName} is thread safe") {
      val singleThreadHashes = (0 until 100).map(i => hash.hash(i.toString.getBytes))
      val multiThreadHashes = Future.sequence((0 until 100).map(i => Future(hash.hash(i.toString.getBytes))))
      singleThreadHashes
        .map(x => Hex.encode(Digest[D].bytes(x)))
        .shouldBe(
          Await
            .result(multiThreadHashes, 1.minute)
            .map(x => Hex.encode(Digest[D].bytes(x)))
        )
    }

  }
}
