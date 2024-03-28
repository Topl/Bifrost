package co.topl.genus.orientDb.schema

import co.topl.brambl.models.Event.{GroupPolicy, SeriesPolicy}
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.models.LockAddress
import co.topl.consensus.models.BlockHeader
import co.topl.genus.services.Txo
import co.topl.node.models.BlockBody
import com.orientechnologies.orient.core.metadata.schema.OClass
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE

@simulacrum.typeclass
trait OIndexable[T] {
  def indexType: OClass.INDEX_TYPE
}

object OIndexable {

  trait Instances {

    val blockHeader: OIndexable[BlockHeader] = new OIndexable[BlockHeader] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val blockHeightHeader: OIndexable[BlockHeader] = new OIndexable[BlockHeader] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.NOTUNIQUE
    }

    val bodyHeader: OIndexable[BlockBody] = new OIndexable[BlockBody] {
      override def indexType: INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val ioTransaction: OIndexable[IoTransaction] = new OIndexable[IoTransaction] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val address: OIndexable[LockAddress] = new OIndexable[LockAddress] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val txo: OIndexable[Txo] = new OIndexable[Txo] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val groupPolicy: OIndexable[GroupPolicy] = new OIndexable[GroupPolicy] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }

    val seriesPolicy: OIndexable[SeriesPolicy] = new OIndexable[SeriesPolicy] {
      override def indexType: OClass.INDEX_TYPE = INDEX_TYPE.UNIQUE
    }
  }
  object Instances extends Instances
}
