package co.topl.genusLibrary.orientDb.schema

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import com.orientechnologies.orient.core.metadata.schema.OClass
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE

@simulacrum.typeclass
trait OrientDbIndexable[T] {
  def indexType: OClass.INDEX_TYPE
}

object OrientDbIndexable {

  trait Instances {

    implicit val blockHeader: OrientDbIndexable[BlockHeader] = new OrientDbIndexable[BlockHeader] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    implicit val ioTransaction: OrientDbIndexable[IoTransaction] = new OrientDbIndexable[IoTransaction] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }
  }
  object Instances extends Instances
}
