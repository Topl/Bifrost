package co.topl.ledger

import co.topl.models.Bytes

import java.nio.charset.StandardCharsets

trait Persistence {
  def read(keys:          List[Bytes]): List[(Bytes, Option[Bytes])]
  def write(version:      Bytes, entries: Iterable[(Bytes, Option[Bytes])]): Persistence
  def rollbackTo(version: Bytes): Persistence
}

object Persistence {

  trait Ops {

    implicit class StringOps(string: String) {
      def bytes: Bytes = Bytes(string.getBytes(StandardCharsets.UTF_8))
    }
  }

  object implicits extends Ops
}

trait PersistenceProvider {
  def persistenceNamed(name: String): Persistence
}
