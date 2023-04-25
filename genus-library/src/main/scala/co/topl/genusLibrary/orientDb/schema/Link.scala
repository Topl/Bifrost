package co.topl.genusLibrary.orientDb.schema

import com.orientechnologies.orient.core.metadata.schema.OType

/**
 * Describe an Link on vertices of a class to other class
 *
 * @param propertyName property for the link
 * @param linkType the type of link @see Defines the type for the link. In the event of an inverse relationship, (the most common), you can specify LINKSET or LINKLIST for 1-n relationships., ...
 * @param linkedClass Defines the class to link to
 * @see https://orientdb.com/docs/last/sql/SQL-Create-Link.html
 */
case class Link(propertyName: String, linkType: OType, linkedClass: String)
