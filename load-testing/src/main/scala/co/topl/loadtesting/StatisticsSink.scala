package co.topl.loadtesting

import akka.NotUsed
import akka.stream.scaladsl.{FileIO, Flow, Sink}
import akka.util.ByteString

import java.nio.file.{Paths, StandardOpenOption}

object StatisticsSink {

  import ToStatisticsCsvLog.ops._

  def apply[T: ToStatisticsCsvLog](path: String): Sink[T, NotUsed] =
    Flow[T]
      .map(_.toLog)
      .map(s => ByteString(s + "\n"))
      .to(FileIO.toPath(Paths.get(path), options = Set(StandardOpenOption.CREATE, StandardOpenOption.APPEND)))
}
