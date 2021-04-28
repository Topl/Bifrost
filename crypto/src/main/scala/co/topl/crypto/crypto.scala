package co.topl

import scala.language.implicitConversions

package object crypto {
  trait BytesOf[A] {
    def get(value: A): Array[Byte]

    def from(bytes: Array[Byte]): A

    def sameElements[B: BytesOf](a: A, b: B): Boolean = get(a) sameElements BytesOf[B].get(b)

    def concat[B: BytesOf](a: A, b: B): Array[Byte] = get(a) ++ BytesOf[B].get(b)

    def prepend(a: A, byte: Byte): Array[Byte] = byte +: get(a)

    def isEmpty(a: A): Boolean = get(a).isEmpty

    def length(a: A): Int = get(a).length

    def take(a: A, num: Int): Array[Byte] = get(a).take(num)

    def slice(a: A, from: Int, to: Int): Array[Byte] = get(a).slice(from, to)

    def map[B](a: A, f: Byte => B): List[B] = get(a).map(f).toList
  }

  object BytesOf {
    def apply[T: BytesOf]: BytesOf[T] = implicitly[BytesOf[T]]
  }
}
