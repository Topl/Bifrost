package co.topl.genusLibrary

/**
 * Code Specifically related to OrientDB
 */
package object orientDb {
  type OrientPropertyKey = String
  type OrientPropertyValue = AnyRef
  type OrientProperties = Map[OrientPropertyKey, OrientPropertyValue]
}
