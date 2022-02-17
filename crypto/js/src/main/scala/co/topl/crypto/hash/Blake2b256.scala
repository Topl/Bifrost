package co.topl.crypto.hash

import co.topl.models.Bytes
import co.topl.models.utility.HasLength.instances.bytesLength
import co.topl.models.utility.{Lengths, Sized}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

/**
 * A thread-unsafe version of the blake2b interface defined above
 */
class Blake2b256 {

  def hash(bytes: Bytes*): Sized.Strict[Bytes, Lengths.`32`.type] = {
    val out = new Array[Byte](32)
    val b2b = blake2b(32)
    bytes.foreach(b => b2b.update(b.toArray))
    b2b.out(out)
    Sized.strictUnsafe(Bytes(out))
  }
}

/**
 * A thread-unsafe version of the blake2b interface defined above
 */
class Blake2b512 {

  def hash(bytes: Bytes*): Sized.Strict[Bytes, Lengths.`64`.type] = {
    val out = new Array[Byte](64)
    val b2b = blake2b(64)
    bytes.foreach(b => b2b.update(b.toArray))
    b2b.out(out)
    Sized.strictUnsafe(Bytes(out))
  }
}

@JSImport("blake2b", JSImport.Namespace)
@js.native
object blake2b extends js.Function1[Int, Blake2bJS] {
  def apply(arg1: Int): Blake2bJS = js.native
}

@JSImport("blake2b", JSImport.Namespace)
@js.native
class Blake2bJS extends js.Object {
  def update(bytes: Array[Byte]): Unit = js.native
  def out(bytes: Array[Byte]): Unit = js.native
}
