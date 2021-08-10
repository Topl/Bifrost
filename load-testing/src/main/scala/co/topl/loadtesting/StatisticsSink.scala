package co.topl.loadtesting

import akka.NotUsed
import akka.stream.scaladsl.{FileIO, Flow, Keep, Sink, Source}
import akka.util.ByteString
import co.topl.modifier.transaction.Transaction.TX

import java.nio.file.{Paths, StandardOpenOption}

object StatisticsSink {

  sealed trait LogPolyTransfer

  case class LogPolyTransferFailure(message: String) extends LogPolyTransfer

  case class LogPolyTransferUnconfirmed(tx: TX) extends LogPolyTransfer

  case class LogPolyTransferSuccess(tx: TX, timeDelta: Int) extends LogPolyTransfer

  def apply(path: String): Sink[LogPolyTransfer, NotUsed] =
    Flow[LogPolyTransfer]
      .map {
        case LogPolyTransferFailure(message) => s"Poly Transfer Broadcast Failure, $message"
        case LogPolyTransferUnconfirmed(tx)  => s"Poly Transfer Not Accepted, ${tx.id}, ${tx.timestamp}"
        case LogPolyTransferSuccess(tx, timeDelta) =>
          s"Poly Transfer Confirmed, ${tx.id}, ${tx.timestamp}, $timeDelta s,"
      }
      .map(s => ByteString(s + "\n"))
      .to(FileIO.toPath(Paths.get(path), options = Set(StandardOpenOption.CREATE, StandardOpenOption.APPEND)))
}
