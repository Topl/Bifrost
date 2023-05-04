package co.topl.genusLibrary.orientDb.schema

/**
 * Describe a type of edge. Currently does not allow for data to be associated with edges, but this may change.
 */

trait EdgeSchema {

  /**
   * The name of the Edge class
   */
  def name: String = "E"
  def label: String
}

object EdgeSchemaInstances {

  // Edge: blockHeader <--> blockHeader
  val blockHeaderEdge: EdgeSchema = new EdgeSchema {
    override def label: String = "hasParent"
  }

  // Edge: blockHeader <--> blockBody
  val blockHeaderBodyEdge: EdgeSchema = new EdgeSchema {
    override def label: String = "hasBody"
  }

  // Edge: blockHeader <--> transactionIO
  val blockHeaderTxIOEdge: EdgeSchema = new EdgeSchema {
    override def label: String = "hasTxIO"
  }

  // Edge: lockAddress <--> transactionIO
  val addressTxIOEdge: EdgeSchema = new EdgeSchema {
    override def label: String = "hasLockAddress"
  }

  // Edge: lockAddress <--> txo
  val addressTxoEdge: EdgeSchema = new EdgeSchema {
    override def label: String = "hasTxo"
  }
}
