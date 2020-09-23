package bifrost.state

import bifrost.modifier.ModifierId
import bifrost.modifier.box.Box
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import io.iohk.iodb.{ ByteArrayWrapper, LSMStore }

import scala.util.Try

trait Registry[K, V] extends StoreInterface with Logging {

  type DB = LSMStore // type of the utxoStore

  /** Helper function to transform registry input key to ByteArrayWrapper */
  def registryInput(key: K): Array[Byte]

  /** Helper function to transform registry output value from Array[Byte] */
  def registryOutput(value: Array[Byte]): V

  /**
   * Lookup value stored by key in the registry
   *
   * @param key storage key used to identify value(s) in registry
   * @return the value associated with the key within the registry
   */
  def lookup(key: K): Seq[V] =
  getFromStorage(registryInput(key))
      .map(_.grouped(storage.keySize).toSeq.map(v => registryOutput(v)))
      .getOrElse(Seq[V]())

  def update( newVersion: VersionTag,
              utxoStore: DB,
              boxIdsToRemove: Set[ModifierId],
              boxesToAdd: Set[Box]): Try[Registry[K, V]]

  def rollbackTo( version: VersionTag): Try[Registry[K, V]]

}
