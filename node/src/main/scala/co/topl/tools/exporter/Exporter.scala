package co.topl.tools.exporter

import co.topl.nodeView.history.History
import co.topl.settings.{AppSettings, StartupOpts}
import co.topl.utils.{Logging, NetworkType}
import co.topl.utils.NetworkType.NetworkPrefix
import mainargs.{arg, main, ParserForMethods}
import co.topl.settings.StartupOptsImplicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object Exporter extends Logging {

  private def initHistory(settings: AppSettings, np: NetworkPrefix): History = History.readOrGenerate(settings)(np)

  @main
  def mongo(
    @arg(name = "uri", doc = "URI of where to connect to an open MongoDB database")
    uriOpt:      Option[String],
    startupOpts: StartupOpts
  ): Unit = {

    val startTime = System.currentTimeMillis()

    val mongo = MongoExport(uriOpt.getOrElse("mongodb://localhost"))
    val (settings, config) = AppSettings.read(startupOpts)
    val history = initHistory(settings, startupOpts.networkTypeOpt.getOrElse(NetworkType.PrivateTestnet).netPrefix)

    val bestBlock = history.bestBlock

    val futures = for {
      height <- 1L to bestBlock.height
      block  <- history.modifierByHeight(height)
    } yield mongo.insert(block.toString)

    Await.ready(Future.sequence(futures), Duration.Inf).onComplete {
      case Failure(exception) => throw exception
      case Success(value) =>
        val totalTime = (System.currentTimeMillis() - startTime) / 1000
        log.info(s"The total time it took to export: ${totalTime / 60} minutes and ${totalTime % 60} seconds")
        mongo.close()
    }
  }

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)

}
