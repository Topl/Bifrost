package co.topl.interpreters

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import cats.effect.{IO, Sync}
import co.topl.catsakka._
import co.topl.models.Bytes
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, EitherValues, Inspectors, OptionValues}

import scala.concurrent.{ExecutionContextExecutor, Future}

class ActorPoolUnsafeResourceSpec
    extends ScalaTestWithActorTestKit
    with Inspectors
    with Matchers
    with MockFactory
    with EitherValues
    with OptionValues
    with BeforeAndAfterAll
    with AnyFlatSpecLike {

  type F[A] = IO[A]

  implicit val ec: ExecutionContextExecutor = testKit.system.executionContext

  // This test is just to demonstrate the thread safety issue
  // "Mutable data that is thread-unsafe" should "result in inconsistent result data" in {
  ignore should "result in inconsistent result data" in {
    val badMutableData = new MutableResource(16)
    val a1 = Array.fill(16)(0: Byte)
    val a2 = Array.fill(16)(1: Byte)
    val f1 = Future { badMutableData.setBytesSlowly(a1); badMutableData.getArrayCopy }
    val f2 = Future { badMutableData.setBytesSlowly(a2); badMutableData.getArrayCopy }
    val (r1, r2) = f1.zip(f2).futureValue
    (Bytes(r1) == Bytes(a1)) shouldBe false
    (Bytes(r2) == Bytes(a2)) shouldBe false
  }

  behavior of "ActorPoolUnsafeResource"

  it should "give thread safety to mutable data" in {
    val underTest =
      ActorPoolUnsafeResource.Eval.make[F, MutableResource](new MutableResource(16), _ => ()).unsafeRunSync()

    val a1 = Array.fill(16)(0: Byte)
    val a2 = Array.fill(16)(1: Byte)
    val f1 = underTest.use(d => Sync[F].delay { d.setBytesSlowly(a1); d.getArrayCopy }).unsafeToFuture()
    val f2 = underTest.use(d => Sync[F].delay { d.setBytesSlowly(a2); d.getArrayCopy }).unsafeToFuture()
    val (r1, r2) = f1.zip(f2).futureValue
    (Bytes(r1) == Bytes(a1)) shouldBe true
    (Bytes(r2) == Bytes(a2)) shouldBe true
  }

}

private class MutableResource(length: Int) {
  private var array = new Array[Byte](length)
  def set(index: Int, byte: Byte): Unit = array(index) = byte

  def setBytesSlowly(newBytes: Array[Byte]): Unit =
    newBytes.zipWithIndex.foreach { case (byte, idx) =>
      set(idx, byte)
      Thread.sleep(100)
    }
  def getArrayCopy: Array[Byte] = array.clone()
}
