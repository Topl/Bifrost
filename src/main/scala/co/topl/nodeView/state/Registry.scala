package co.topl.nodeView.state

import co.topl.nodeView.state.MinimalState.VersionTag
import co.topl.nodeView.state.box.{BoxId, GenericBox}
import co.topl.nodeView.state.box.proposition.Proposition
import co.topl.utils.Logging

import scala.reflect.ClassTag
import scala.util.Try

trait Registry[K, V] extends StoreInterface with Logging {

  type SR = StateReader[_<: GenericBox[_ <: Proposition, _]]

  protected def update (newVersion: VersionTag, toRemove: Map[K, Seq[V]], toAppend: Map[K, Seq[V]]): Try[Registry[K, V]]

  protected def rollbackTo (version: VersionTag): Try[Registry[K, V]]

  /** Helper function to transform registry input key to Array[Byte] */
  protected val registryInput: K => Array[Byte]

  /** Helper function to transform registry output value from Array[Byte] */
  protected val registryOutput: Array[Byte] => Seq[V]

  /** Helper function to transform registry output value to input for state */
  protected val registryOut2StateIn: (K, V) => BoxId

  /**
    * Lookup boxId stored by key in the registry
    *
    * @param key storage key used to identify value(s) in registry
    * @return the value associated with the key within the registry
    */
  def lookup (key: K): Option[Seq[BoxId]] =
    getFromStorage(registryInput(key))
      .map(registryOutput)
      .map(_.map(registryOut2StateIn(key, _)))

  /**
   * Lookup raw value stored in the registry
   * @param key storage key used to identify value(s) in the registry
   * @return the raw value stored in the registry
   */
  def lookupRaw (key: K): Option[Seq[V]] =
    getFromStorage(registryInput(key))
      .map(registryOutput)

  /**
    * A convenience method to allow for seamlessly looking up a box in a registry and then querying
    * for the box data in state
    *
    * @param key   the registry key used to lookup boxIds in the registry
    * @param state the state containing the token box data to be retrieved
    * @return a sequence of boxes from state using the registry key to look them up
   */
  protected def getBox[BX : ClassTag] (key: K, state: SR): Option[Seq[BX]] =
    lookup(key)
      .map {
        _.map(state.getBox)
          .flatMap {
            case Some(box: BX) => Some(box)
            case _             => None
          }
      }
}
