package co.topl.modifier

import cats.implicits._

/**
 * Object that contains modifiers of type `MOD`
 */
trait ContainsModifiers[MOD <: NodeViewModifier] {

  /**
   * @param persistentModifier -  modifier to check
   * @return `true` if this object contains this modifier, `false` otherwise
   */
  def contains(persistentModifier: MOD): Boolean =
    modifierById(persistentModifier.id).isDefined

  /**
   * @param id -  modifier's id
   * @return `true` if this object contains modifier with specified id, `false` otherwise
   */
  def contains(ids: Seq[ModifierId]): Boolean =
    ids.forall(id => modifierById(id).isDefined)

  /**
   * @param id -  modifier's id
   * @return `true` if this object contains modifier with specified id, `false` otherwise
   */
  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  protected def persistenceAccessors: Iterator[ModifierId => Option[MOD]]

  /**
   * @param modifierId - modifier id to get
   * @return modifier of type MOD with id == modifierId if exist
   */
  def modifierById(modifierId: ModifierId): Option[MOD] =
    persistenceAccessors.foldLeft(none[MOD]) {
      case (None, check) => check(modifierId)
      case (some, _)     => some
    }
}
