package bifrost.state

import bifrost.modifier.box.GenericBox
import bifrost.modifier.box.proposition.Proposition
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging

import scala.util.Try

trait Registry[K, V] extends StoreInterface with Logging {

  /** Helper function to transform registry input key to ByteArrayWrapper */
  def registryInput(key: K): Array[Byte]

  /** Helper function to transform registry output value from Array[Byte] */
  def registryOutput(value: Array[Byte]): V

  /**
   * Lookup boxId stored by key in the registry
   *
   * @param key storage key used to identify value(s) in registry
   * @return the value associated with the key within the registry
   */
  def lookup(key: K): Seq[V] = {
    getFromStorage(registryInput(key))
      .map(_.grouped(BoxId.size).toSeq.map(v => registryOutput(v)))
      .getOrElse(Seq[V]())
  }

  def update(newVersion: VersionTag, toRemove: Map[K, Seq[V]], toAppend: Map[K, Seq[V]] ): Try[Registry[K, V]]

  def rollbackTo( version: VersionTag): Try[Registry[K, V]]

}
