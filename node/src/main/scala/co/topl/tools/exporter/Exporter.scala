package co.topl.tools.exporter

import co.topl.nodeView.history.History
import co.topl.settings.{AppSettings, StartupOpts}
import co.topl.utils.{Logging, NetworkType}
import co.topl.utils.NetworkType.NetworkPrefix
import mainargs.{arg, main, ParserForMethods}
import co.topl.settings.StartupOptsImplicits._
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object Exporter extends Logging {

  private def initHistory(settings: AppSettings, np: NetworkPrefix): History = History.readOrGenerate(settings)(np)

  def flattenFields(value: Json): Option[Iterable[(String, Json)]] = value.asObject.map(
    _.toIterable.flatMap { case (k, v) =>
      flattenFields(v) match {
        case None => List(k -> v)
        case Some(fields) =>
          fields.map { case (k, v) => k -> v }
      }
    }
  )

  private def export(connection: Exportable, history: History): Unit = {

    val startTime = System.currentTimeMillis()
    val bestBlock = history.bestBlock

    val futures: IndexedSeq[Future[_]] = connection.dataType match {
      case DataType.Block =>
        for {
          height    <- 1L to bestBlock.height
          block     <- history.modifierByHeight(height)
          blockJson <- flattenFields(block.asJson)
        } yield {
          val formattedBlock: Json = flattenFields(block.asJson).fold(block.asJson)(Json.fromFields)
          connection.insert(Seq(formattedBlock.toString))
        }
      case DataType.Transaction =>
        for {
          height <- 1L to bestBlock.height
          block  <- history.modifierByHeight(height)
          tx     <- Option(block.transactions)
        } yield connection.insert(tx.map(_.asJson.toString))
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
    dataType:    String = "block",
    startupOpts: StartupOpts
  ): Unit = {

    val dt = DataType.getDataType(dataType) match {
      case Some(value) => value
      case None        => throw new Exception(s"An invalid data type was provided")
    }

    val mongo = MongoExport(uriOpt.getOrElse("mongodb://localhost"), dt)
    val (settings, config) = AppSettings.read(startupOpts)
    val history = initHistory(settings, startupOpts.networkTypeOpt.getOrElse(NetworkType.PrivateTestnet).netPrefix)

    export(mongo, history)
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)

}
