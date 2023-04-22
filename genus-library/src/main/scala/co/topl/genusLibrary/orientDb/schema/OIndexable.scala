package co.topl.genusLibrary.orientDb.schema

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.Address
import co.topl.consensus.models.BlockHeader
import com.orientechnologies.orient.core.metadata.schema.OClass
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE

@simulacrum.typeclass
trait OIndexable[T] {
  def indexType: OClass.INDEX_TYPE
}

object OIndexable {

  trait Instances {

    implicit val blockHeader: OIndexable[BlockHeader] = new OIndexable[BlockHeader] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    implicit val ioTransaction: OIndexable[IoTransaction] = new OIndexable[IoTransaction] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    implicit val address: OIndexable[Address] = new OIndexable[Address] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }
  }
  object Instances extends Instances
}
