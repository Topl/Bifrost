package co.topl.algebras

import scala.concurrent.duration.Duration

trait Cache[F[_], T] {

  def cachingF(key: String)(ttl: Option[Duration])(f: F[T]): F[T]

  def get(key: String): F[Option[T]]

  def put(key: String)(value: T): F[Unit]

}
