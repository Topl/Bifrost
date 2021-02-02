package co.topl.http.api

/** A case class used in AppSettings to note the state of the differnet API endpoints*/
case class NamespaceSelector(
  private val topl:  Boolean = false,
  private val util:  Boolean = false,
  private val admin: Boolean = false,
  private val debug: Boolean = false
) {

  lazy val namespaceStates: Map[Namespace, Boolean] =
    Map(
      ToplNamespace -> topl,
      UtilNamespace -> util,
      AdminNamespace -> admin,
      DebugNamespace -> debug
    )
}

/** a generic class to be shared by all namespaces that provides a common name value used for pattern matching
  * in the end-point handler partial function */
sealed abstract class Namespace(val name: String)

case object ToplNamespace extends Namespace("topl")
case object UtilNamespace extends Namespace("util")
case object AdminNamespace extends Namespace("admin")
case object DebugNamespace extends Namespace("debug")
