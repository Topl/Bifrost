package co.topl.interpreters

import cats.effect.{IO, Sync}
import cats.implicits._
import co.topl.models.Bytes
import munit.CatsEffectSuite

class CatsUnsafeResourceSpec extends CatsEffectSuite {

  type F[A] = IO[A]

  test("give thread safety to mutable data") {
    val a1 = Array.fill(16)(0: Byte)
    val a2 = Array.fill(16)(1: Byte)
    for {
      underTest <- CatsUnsafeResource.make[F, MutableResource](new MutableResource(16), 1)
      (r1, r2) <- (
        underTest.use(d => Sync[F].delay { d.setBytesSlowly(a1); d.getArrayCopy }),
        underTest.use(d => Sync[F].delay { d.setBytesSlowly(a2); d.getArrayCopy })
      ).parTupled
      _ = assert(Bytes(r1) == Bytes(a1))
      _ = assert(Bytes(r2) == Bytes(a2))
    } yield ()
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
