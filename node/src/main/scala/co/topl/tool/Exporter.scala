package co.topl.tool

import co.topl.nodeView.history.History
import co.topl.settings.{AppSettings, StartupOpts}
import co.topl.tools.exporter.{DataType, Exportable, MongoExport}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import co.topl.utils.{Logging, NetworkType}
import io.circe.Json
import mainargs.{arg, main, ParserForMethods}
import co.topl.settings.StartupOptsImplicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object Exporter extends Logging {

  private def initHistory(settings: AppSettings, np: NetworkPrefix): History = History.readOrGenerate(settings)(np)

  private def export(connection: Exportable, history: History, start: Long = 1L, end: Long): Unit = {

    val startTime = System.currentTimeMillis()

    val futures: IndexedSeq[Future[_]] = connection.dataType match {
      case DataType.Block =>
        for {
          height <- start to end
          block  <- history.modifierByHeight(height)
        } yield connection.insert(Seq(BlockDataModel(block)))
      case DataType.Transaction =>
        for {
          height <- start to end
          block  <- history.modifierByHeight(height)
        } yield connection.insert(
          block.transactions.map(tx => ConfirmedTransactionDataModel(block.id.toString, block.height, tx))
        )
      // TODO: Decide if the set of all boxes that have existed or the current set of boxes should be returned
      case DataType.Box => ???
    }

    Await.ready(Future.sequence(futures), Duration.Inf).onComplete {
      case Failure(exception) => throw exception
      case Success(value) =>
        val totalTime = (System.currentTimeMillis() - startTime) / 1000
        log.info(s"The total time it took to export: ${totalTime / 60} minutes and ${totalTime % 60} seconds")
        connection.close()
    }
  }

  @main
  def mongo(
    @arg(name = "uri", doc = "URI of where to connect to an open MongoDB database")
    uriOpt: Option[String],
    @arg(
      name = "type",
      short = 't',
      doc = "Specify type of data to export e.g. 'blocks', 'transactions', or 'boxes'. Default is block"
    )
    dataType: String = "block",
    @arg(name = "start", doc = "The block height to start from")
    start: Option[Long],
    @arg(name = "end", doc = "The block height to end on")
    end: Option[Long],
    @arg(name = "database", doc = "The name of the database to connect to")
    database: Option[String],
    @arg(name = "collection", doc = "The name of the collection to write to")
    collection:  Option[String],
    startupOpts: StartupOpts
  ): Unit = {

    val dt = DataType.getDataType(dataType) match {
      case Some(value) => value
      case None        => throw new Exception(s"An invalid data type was provided")
    }

    val mongo =
      MongoExport(
        uriOpt.getOrElse("mongodb://localhost"),
        database.getOrElse("bifrost"),
        collection.getOrElse(dt.name),
        dt
      )
    val (settings, config) = AppSettings.read(startupOpts)
    val history = initHistory(settings, startupOpts.networkTypeOpt.getOrElse(NetworkType.PrivateTestnet).netPrefix)

    export(mongo, history, start.getOrElse(1L), end.getOrElse(history.bestBlock.height))
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)

}
