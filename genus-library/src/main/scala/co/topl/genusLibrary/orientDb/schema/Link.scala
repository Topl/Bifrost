package co.topl.genusLibrary.orientDb.schema

import com.orientechnologies.orient.core.metadata.schema.{OClass, OType}

/**
 * Describe an index on vertices of a class
 *
 * @param propertyName The name of the index // TODO
 * @param linkType the type of link @see Defines the type for the link. In the event of an inverse relationship, (the most common), you can specify LINKSET or LINKLIST for 1-n relationships., ...
 * @param linkedClass The names of the properties whose values are used to construct index entries. // TODO
 */
case class Link(propertyName: String, linkType: OType, linkedClass: OClass)
