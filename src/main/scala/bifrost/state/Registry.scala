package bifrost.state

import bifrost.modifier.box.GenericBox
import bifrost.modifier.box.proposition.Proposition
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging

import scala.reflect.ClassTag
import scala.util.{Success, Try}

trait Registry[K, V] extends StoreInterface with Logging {

  type SR = StateReader[_<: GenericBox[_ <: Proposition, _]]

  protected def update (newVersion: VersionTag, toRemove: Map[K, Seq[V]], toAppend: Map[K, Seq[V]]): Try[Registry[K, V]]

  protected def rollbackTo (version: VersionTag): Try[Registry[K, V]]

  /** Helper function to transform registry input key to Array[Byte] */
  protected def registryInput (key: K): Array[Byte]

  /** Helper function to transform registry output value from Array[Byte] */
  protected def registryOutput (value: Array[Byte]): V

  /** Helper function to transform registry output value to input for state */
  protected def registryOut2StateIn (value: V): Array[Byte]

  /**
    * Lookup boxId stored by key in the registry
    *
    * @param key storage key used to identify value(s) in registry
    * @return the value associated with the key within the registry
    */
  def lookup (key: K): Seq[V] = {
    getFromStorage(registryInput(key))
      .map(_.grouped(BoxId.size).toSeq.map(v => registryOutput(v)))
      .getOrElse(Seq[V]())
  }

  /**
    * A convenience method to allow for seamlessly looking up a box in a registry and then querying
    * for the box data in state
    *
    * @param key   the registry key used to lookup boxIds in the registry
    * @param state the state containing the token box data to be retrieved
    * @return a sequence of boxes from state using the registry key to look them up
    */
  protected def getBox[BX : ClassTag] (key: K, state: SR): Option[Seq[BX]] = {
    log.info("Looking up boxes")
    val g =
      Try {
      lookup(key)
        .map(v => state.getBox(registryOut2StateIn(v)))
    }.toOption

    println(s">>>>>>> ${g.getOrElse(None)}")

      g.map { _.flatMap {
      case Some(box: BX) => {
        log.info("Some boxes")
        Some(box)
      }
      case _             => {
        log.info("No boxes")
        None
      }
    }
    }
  }
}
