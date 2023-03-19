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

  // Edge: blockHeader <--> blockHeader
  val blockHeaderEdgeSchema: EdgeSchema = new EdgeSchema {
    override def name: String = "BlockHeaderEdge"
  }

  // Edge: blockHeader <--> blockBody
  val blockHeaderBodyEdgeSchema: EdgeSchema = new EdgeSchema {
    override def name: String = "BlockHeaderBodyEdge"
  }
}
