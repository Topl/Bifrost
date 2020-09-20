package bifrost.state

import bifrost.modifier.ModifierId
import bifrost.modifier.box.Box
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

import scala.util.Try

trait Registry[K, V] extends Logging {

  type DB = LSMStore // type of the utxoStore

  val storage: LSMStore

  /** Public method to close storage on termination */
  def closeStorage(): Unit = storage.close()

  /** Helper function to transform registry input key to ByteArrayWrapper */
  def registryInput(key: K): ByteArrayWrapper

  /** Helper function to transform registry output value from Array[Byte] */
  def registryOutput(value: Array[Byte]): V

  /** Helper function to transform UTXO input value to ByteArrayWrapper */
  def utxoInput(value: V): ByteArrayWrapper

  /**
   * Lookup value stored by key in the registry
   *
   * @param key storage key used to identify value(s) in registry
   * @return the value associated with the key within the registry
   */
  def lookup(key: K): Seq[V] =
    storage
      .get(registryInput(key))
      .map(_.data.grouped(storage.keySize).toSeq.map(v => registryOutput(v)))
      .getOrElse(Seq[V]())

  /**
   * Retrieve a sequence of boxes from the UTXO set based on the given key
   *
   * @param key storage key used to identify value(s) in registry
   * @param utxoStore storage database containing the UTXO set
   * @return a sequence of boxes stored beneath the specified key
   */
  def boxesByKey(key: K, utxoStore: DB): Seq[Box] = {
    lookup(key)
      .map(v => {
        utxoStore.get(utxoInput(v))
          .map(_.data)
          .map(BoxSerializer.parseBytes)
          .flatMap(_.toOption)
      })
      .filter {
        case _: Some[Box] => true
        case None => false
      }
      .map(_.get)
  }

  def update( newVersion: VersionTag,
              utxoStore: DB,
              boxIdsToRemove: Set[ModifierId],
              boxesToAdd: Set[Box]): Try[Registry[K, V]]

  def rollbackTo( version: VersionTag): Try[Registry[K, V]]

}
