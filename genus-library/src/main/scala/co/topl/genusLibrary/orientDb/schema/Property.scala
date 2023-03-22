package co.topl.genusLibrary.orientDb.schema

import com.orientechnologies.orient.core.metadata.schema.OType

/**
 * Represents an individual piece of data that will be stored in a class of vertices
 *
 * @param name The name of the property
 * @param propertyType The datatype of the property
 */
case class Property(
  name:         String,
  propertyType: OType,
  mandatory:    Boolean,
  readOnly:     Boolean,
  notNull:      Boolean
)
