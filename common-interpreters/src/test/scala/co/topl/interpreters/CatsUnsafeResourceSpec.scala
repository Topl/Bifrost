package co.topl.interpreters

import cats.effect.IO
import cats.effect.Sync
import cats.implicits._
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
      _ = assert(r1 sameElements a1)
      _ = assert(r2 sameElements a2)
    } yield ()
  }

}

private class MutableResource(length: Int) {
  private val array = new Array[Byte](length)
  def set(index: Int, byte: Byte): Unit = array(index) = byte

  def setBytesSlowly(newBytes: Array[Byte]): Unit =
    newBytes.zipWithIndex.foreach { case (byte, idx) =>
      set(idx, byte)
      Thread.sleep(100)
    }
  def getArrayCopy: Array[Byte] = array.clone()
}
