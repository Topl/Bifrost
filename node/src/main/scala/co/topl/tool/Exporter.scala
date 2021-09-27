package co.topl.tool

import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.history.History
import co.topl.rpc.implicits.client.int128Encoder
import co.topl.settings.{AppSettings, StartupOpts}
import co.topl.tools.exporter.{DataType, Exportable, MongoExport}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.{Logging, NetworkType}
import co.topl.settings.StartupOptsImplicits._
import io.circe.Json
import io.circe.syntax.EncoderOps
import mainargs.{arg, main, ParserForMethods}
import co.topl.utils.codecs.implicits.digest32JsonEncoder
import io.circe.{Encoder, Json}
import io.circe.syntax._

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
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

  def formatISO8601(timestamp: Long): String = {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSxxx")
    Instant.ofEpochMilli(timestamp).atZone(ZoneId.of("UTC")).format(formatter)
  }

  def formatTimestamp(tx: Transaction.TX): Json =
    tx.asJson.hcursor.downField("timestamp").withFocus(_.withNumber(_.toString.asJson)).top.get

  implicit val blockEncoder: Encoder[Block] = { b: Block =>
    val totalFees = b.transactions.map(_.fee).sum

    Map(
      "id"              -> b.id.toString.asJson,
      "parentId"        -> b.parentId.toString.asJson,
      "timestamp"       -> b.timestamp.toString.asJson,
      "generatorBox"    -> b.generatorBox.asJson,
      "publicKey"       -> b.publicKey.asJson,
      "signature"       -> b.signature.asJson,
      "height"          -> b.height.asJson,
      "difficulty"      -> b.difficulty.toString.asJson,
      "txRoot"          -> b.merkleTree.rootHash.asJson,
      "bloomFilter"     -> b.bloomFilter.asJson,
      "version"         -> b.version.asJson,
      "numTransactions" -> b.transactions.length.asJson,
      "blockSize"       -> b.bytes.length.asJson,
      "fees"            -> totalFees.toString.asJson
    ).asJson
  }

  def txFormat(block: Block, tx: Transaction.TX): Json = {
    val timestamp = tx.timestamp.toString.asJson
    val timestampFormat: Json =
      tx.asJson.hcursor.downField("timestamp").set(timestamp).top.get
    val blockInfo: Json = Map("block" -> Map("id" -> block.id.asJson, "height" -> block.height.asJson)).asJson
    timestampFormat.deepMerge(blockInfo)
  }

  private def export(connection: Exportable, history: History, start: Long = 1L, end: Long): Unit = {

    val startTime = System.currentTimeMillis()

    val futures: IndexedSeq[Future[_]] = connection.dataType match {
      case DataType.Block =>
        for {
          height <- start to end
          block  <- history.modifierByHeight(height)
        } yield {
          val formattedBlock: Json = block.asJson(blockEncoder)
          connection.insert(Seq(formattedBlock.toString))
        }
      case DataType.Transaction =>
        for {
          height <- start to end
          block  <- history.modifierByHeight(height)
        } yield connection.insert(block.transactions.map(tx => txFormat(block, tx).toString))
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
