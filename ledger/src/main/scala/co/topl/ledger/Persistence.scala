package co.topl.ledger

import co.topl.models.Bytes

trait Persistence {
  def read(keys:          List[Bytes]): List[(Bytes, Option[Bytes])]
  def write(version:      Bytes, entries: Iterable[(Bytes, Option[Bytes])]): Persistence
  def rollbackTo(version: Bytes): Persistence
}

trait PersistenceProvider {
  def persistenceNamed(name: String): Persistence
}
