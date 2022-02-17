package co.topl

import cats.implicits._
import cats.{Order, Show}
import scalacache.CacheKeyBuilder

package object consensus {

  implicit class OrderSupport[T](order: Order[T]) {

    def tiebreakWith(other: Order[T]): Order[T] =
      Order.whenEqual(order, other)
  }

  def cacheKeyBuilder[T: Show]: CacheKeyBuilder = new CacheKeyBuilder {

    def toCacheKey(parts: Seq[Any]): String =
      parts.map {
        case s: T @unchecked => s.show
        case s               => throw new MatchError(s)
      }.mkString

    def stringToCacheKey(key: String): String = key
  }
}
