package co.topl.tools.exporter

sealed abstract class DataType(val name: String)

object DataType {
  case object Block extends DataType("blocks")

  case object Transaction extends DataType("transactions")

  case object Box extends DataType("boxes")

  lazy val all: Seq[DataType] = Seq(Block, Transaction, Box)

  def getDataType(dt: String): Option[DataType] = all.find(_.name == dt)
}
