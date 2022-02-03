package co.topl.nodeView.history

import co.topl.nodeView.KeyValueStore

class InMemoryKeyValueStore extends KeyValueStore {
  import InMemoryKeyValueStore._
  var state: Map[WrappedBytes, Array[Byte]] = Map.empty
  var changes: List[ChangeSet] = Nil

  override def update(
    version:  Array[Byte],
    toRemove: Iterable[Array[Byte]],
    toAdd:    Iterable[(Array[Byte], Array[Byte])]
  ): Unit = {
    val changeSet = ChangeSet(
      version,
      toRemove.toList.map(key => Remove(key, state(new WrappedBytes(key)))) ++ toAdd.map { case (key, value) =>
        if (state.contains(new WrappedBytes(key)))
          Update(key, state(new WrappedBytes(key)), value)
        else
          Insert(key, value)
      }
    )
    state --= toRemove.map(new WrappedBytes(_))
    state ++= toAdd.map { case (k, v) => new WrappedBytes(k) -> v }
    changes :+= changeSet
  }

  override def rollbackTo(version: Array[Byte]): Unit = {
    require(changes.exists(_.version sameElements version))
    def revertLatest(): Unit = {
      val latest = changes.last
      changes = changes.init
      latest.changes.foreach {
        case Insert(key, _) =>
          state -= new WrappedBytes(key)
        case Update(key, previous, _) =>
          state += (new WrappedBytes(key) -> previous)
        case Remove(key, previous) =>
          state += (new WrappedBytes(key) -> previous)
      }
    }
    while (changes.nonEmpty && !java.util.Arrays.equals(changes.last.version, version)) revertLatest()
  }

  override def get(key: Array[Byte]): Option[Array[Byte]] =
    state.get(new WrappedBytes(key))

  override def close(): Unit = {}

  override def latestVersionId(): Option[Array[Byte]] =
    changes.lastOption.map(_.version)
}

object InMemoryKeyValueStore {

  def empty(): InMemoryKeyValueStore = new InMemoryKeyValueStore

  case class ChangeSet(version: Array[Byte], changes: List[Change])
  sealed abstract class Change
  case class Insert(key: Array[Byte], value: Array[Byte]) extends Change
  case class Update(key: Array[Byte], previous: Array[Byte], value: Array[Byte]) extends Change
  case class Remove(key: Array[Byte], previous: Array[Byte]) extends Change

  class WrappedBytes(val bytes: Array[Byte]) {

    override def equals(obj: Any): Boolean = obj match {
      case o: WrappedBytes => java.util.Arrays.equals(bytes, o.bytes)
      case _               => false
    }

    override def hashCode(): Int = java.util.Arrays.hashCode(bytes)
  }
}
