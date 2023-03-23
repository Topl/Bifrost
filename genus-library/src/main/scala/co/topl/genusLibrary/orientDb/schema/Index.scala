package co.topl.genusLibrary.orientDb.schema

import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE

/**
 * Describe an index on vertices of a class
 *
 * @param name The name of the index
 * @param indexType the type of index @see [[INDEX_TYPE.UNIQUE]], [[INDEX_TYPE.NOTUNIQUE]], ...
 * @param propertyNames The names of the properties whose values are used to construct index entries.
 */
case class Index(name: String, indexType: INDEX_TYPE, propertyNames: String*)
