package co.topl.storage.generic

import akka.{Done, NotUsed}
import akka.stream.scaladsl.{Sink, Source}
import cats.data.EitherT

import scala.concurrent.Future

class GenericStore {}

object GenericStore {
  sealed abstract class Error
  case class ThrowableError(throwable: Throwable) extends Error
}

trait SetStore[T] {
  def contains(t: T): EitherT[Future, GenericStore.Error, Boolean]
  def put(t:      T): EitherT[Future, GenericStore.Error, Boolean]
  def remove(t:   T): EitherT[Future, GenericStore.Error, Boolean]
  def values(): Source[T, NotUsed]
  def putMany(): Sink[T, Future[Done]]
}

trait MapStore[K, V] {
  def contains(k: K): EitherT[Future, GenericStore.Error, Boolean]
  def get(k:      K): EitherT[Future, GenericStore.Error, Option[V]]
  def put(k:      K, v: V): EitherT[Future, GenericStore.Error, Boolean]
  def remove(k:   K): EitherT[Future, GenericStore.Error, Boolean]
  def keys(): Source[K, NotUsed]
  def values(): Source[V, NotUsed]
  def keyValues(): Source[(K, V), NotUsed]
  def putMany(): Sink[(K, V), Future[Done]]
}
