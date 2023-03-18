package co.topl.genusLibrary.orientDb.schema

/**
 * Describe a type of edge. Currently does not allow for data to be associated with edges, but this may change.
 */

trait EdgeSchema {

  /**
   * The name of the Edge class
   */
  def name: String
}

object EdgeSchemaInstances {

  val blockHeaderEdgeSchema: EdgeSchema = new EdgeSchema {
    override def name: String = "BlockHeaderEdge"
  }
}
