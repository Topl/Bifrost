package co.topl.byzantine

import cats.effect.{Async, IO}
import munit.CatsEffectSuite
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fastparse.{CharsWhile, _}
import SingleLineWhitespace._
import fs2.Stream

class LogParserTest extends CatsEffectSuite {
  type F[A] = IO[A]

  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  private val logLine =
    "19:35:40.902| INFO  Bifrost.RPC.Server - Processed Transaction id=t_2JrfPrQHwtgFeVY8QVVVbRq8mrwmvChgN6VTHct5PFCX from RPC"

  private val logMultiLine =
    """
      |19:35:40.840| WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=t_BArDYJWYH9mxwmQVXGaonPVUWsxmCwfpoy8YDbH5Vhox reasons=NonEmptyChain(InvalidProofType)
      |11:05:28.481| INFO  Bifrost.BlockProducer - Minted header=BlockHeader(id=b_3AhbR6xTnohZYRcJNnBUtDc1XD9Lgi8RoFoMSUgGBDya parentId=b_8RTTnrjCCydjipm59kTsfhFenxBLvS563K1S3HXbZRif parentSlot=178 timestamp=2023-09-05T11:05:28.582Z height=12 slot=180 address=74ifm7n7zRDXLaaa9NpfAtynUGrmm3BLqxQrE8s9ejNn) body=Body(transactionIds=[1]List(t_hbg5oyDyETuJv4n12U7ZzF43BV2vMxtU5MHpUrzUw3R), reward=Some(t_7oPCJT7542UtaDVqWgxPXhqey9NxGxfKVUQ74NpwDoGo))
      |19:36:03.751| INFO  Bifrost.BlockProducer - Minted header=BlockHeader(id=b_3zKdJbNa4nsNaxUFdDBLtanKLXfvYhUDsJPUD3HbAXSE parentId=b_HsoWhEwN3FDEqSrYfxeGBYYc9QCZjC3UQNhEJF71iwJB parentSlot=285 timestamp=2023-08-31T19:36:03.777Z height=19 slot=297 address=J6YnxN9Wpp3AauDhR7p6QDgKC3gCGtpFPeSGtERYoqpi) body=Body(transactionIds=[0]List(), reward=None)
      |19:35:43.072| INFO  Bifrost.BlockProducer - Minted header=BlockHeader(id=b_C4EBMQdGazjV96ot7mm5hyndRc1syLbFZH34v2VVae2P parentId=b_7JDurV9bCkDxnNewbXn27gwAQmpg8iLkYe5zhiEgoP5d parentSlot=188 timestamp=2023-08-31T19:35:43.177Z height=10 slot=194 address=J6YnxN9Wpp3AauDhR7p6QDgKC3gCGtpFPeSGtERYoqpi) body=Body(transactionIds=[5]List(t_BYqS7cKvB2kt2i9VLycDzaYgro9yHqeP9JAFd6gKApKB, t_2JrfPrQHwtgFeVY8QVVVbRq8mrwmvChgN6VTHct5PFCX, t_5oYZqrtjL4zgpQpCunwJLd4X2dtE8RcbbgnbetD3R3Xa, t_64fMppN9MrVib1wCu44XGrfnj8FPjXxgvDwdvKSV5igC, t_BZzN8q92NFbhrZVMzVpxhHFjhz1g793aEMLZjsBjDtdR), reward=None)
      |19:35:40.869| WARN  Bifrost.RPC.Server - Received syntactically invalid transaction id=t_8LjbLtZ2zHVsrqTUFP8eYMbEtLA7mfd4ckHCGP353Z1K reasons=NonEmptyChain(InvalidProofType)
      |19:35:40.902| INFO  Bifrost.RPC.Server - Processed Transaction id=t_2JrfPrQHwtgFeVY8QVVVbRq8mrwmvChgN6VTHct5PFCX from RPC
      |19:35:41.487| INFO  Bifrost.RPC.Server - Processed Transaction id=t_4Ma2zzL2zNpkgjAsNbXX3X1KVzWVrtwEpj8Xg5EwseqL from RPC
      |19:30:46.029| INFO  Bifrost.RPC.Server - Processed Transaction id=t_vE2oDC9mAksnsRrskT4sNYvQ5FfsPpUZmtZtB9UtYcH from RPC
    """.stripMargin

  test("transactionId") {
    val parsedLine = parse("t_2JrfPrQHwtgFeVY8QVVVbRq8mrwmvChgN6VTHct5PFCX", LogParserTest.transactionId(_))
    assert(parsedLine.get.value == "t_2JrfPrQHwtgFeVY8QVVVbRq8mrwmvChgN6VTHct5PFCX")
  }

  test("Parse Line") {
    val parsedLine = parse(logLine, LogParserTest.processedFromRpc(_))
    assert(parsedLine.get.value == "t_2JrfPrQHwtgFeVY8QVVVbRq8mrwmvChgN6VTHct5PFCX")
  }

  test("Parse multiple lines processedFromRpc") {

    val parsedLines = logMultiLine.linesIterator
      .map(line => parse(line, LogParserTest.processedFromRpc(_)))
      .collect { case Parsed.Success(value, _) => value }
      .toSeq

    assert(
      parsedLines == Seq(
        "t_2JrfPrQHwtgFeVY8QVVVbRq8mrwmvChgN6VTHct5PFCX",
        "t_4Ma2zzL2zNpkgjAsNbXX3X1KVzWVrtwEpj8Xg5EwseqL",
        "t_vE2oDC9mAksnsRrskT4sNYvQ5FfsPpUZmtZtB9UtYcH"
      )
    )
  }

  test("Parse multiple lines receivedSyntacticallyInvalid") {
    val parsedLines = logMultiLine.linesIterator
      .map(line => parse(line, LogParserTest.receivedSyntacticallyInvalid(_)))
      .collect { case Parsed.Success(value, _) => value }
      .toSeq

    assert(
      parsedLines == Seq(
        "t_BArDYJWYH9mxwmQVXGaonPVUWsxmCwfpoy8YDbH5Vhox",
        "t_8LjbLtZ2zHVsrqTUFP8eYMbEtLA7mfd4ckHCGP353Z1K"
      )
    )
  }

  test("Parse list of minted transaction Ids") {
    val parsedLines = logMultiLine.linesIterator
      .map(line => parse(line, LogParserTest.minted(_)))
      .collect { case Parsed.Success(value, _) => value }
      .toSeq

    val txIds = parsedLines.flatMap(s => s.split(",")).filter(_.nonEmpty).map(_.trim)
    assert(
      txIds == Seq(
        "t_hbg5oyDyETuJv4n12U7ZzF43BV2vMxtU5MHpUrzUw3R",
        "t_BYqS7cKvB2kt2i9VLycDzaYgro9yHqeP9JAFd6gKApKB",
        "t_2JrfPrQHwtgFeVY8QVVVbRq8mrwmvChgN6VTHct5PFCX",
        "t_5oYZqrtjL4zgpQpCunwJLd4X2dtE8RcbbgnbetD3R3Xa",
        "t_64fMppN9MrVib1wCu44XGrfnj8FPjXxgvDwdvKSV5igC",
        "t_BZzN8q92NFbhrZVMzVpxhHFjhz1g793aEMLZjsBjDtdR"
      ),
      txIds
    )
  }

  test("parse processedFromRpc stream multiline") {
    val logs =
      fs2.Stream
        .emits(logMultiLine.getBytes)
        .covary[F]
        .through(fs2.text.utf8.decode)
        .through(fs2.text.lines)
        .map(line => parse(line, LogParserTest.processedFromRpc(_)))
        .collect { case Parsed.Success(value, _) =>
          value
        }
        .compile
        .toList

    assertIO(
      logs,
      List(
        "t_2JrfPrQHwtgFeVY8QVVVbRq8mrwmvChgN6VTHct5PFCX",
        "t_4Ma2zzL2zNpkgjAsNbXX3X1KVzWVrtwEpj8Xg5EwseqL",
        "t_vE2oDC9mAksnsRrskT4sNYvQ5FfsPpUZmtZtB9UtYcH"
      )
    )

  }

}

object LogParserTest {

  private def time[$: P]: P[Unit] = P(AnyChar.rep(max = 12)) // consumes 19:35:40.902

  private def level[$: P]: P[Unit] = P("INFO" | "WARN" | "DEBUG" | "ERROR")
  private def packagePath[$: P]: P[Unit] = P(CharsWhile(_ != ' '))

  private def b58[$: P]: P[Unit] = P(CharsWhileIn("1-9A-HJ-NP-Za-km-z"))
  private def transactionId[$: P]: P[String] = P("t_" ~ b58).!
  private def blockIdId[$: P]: P[String] = P("b_" ~ b58).!

  private def seqCount[$: P]: P[Int] = P("[" ~ P(CharsWhileIn("0-9")).!.map(_.toInt) ~ P("]"))

  private def seqTransactionId[$: P]: P[Seq[String]] = P("t_" ~ b58).!.rep(sep = P(","))

  private def processedFromRpc[$: P]: P[String] = P(
    time ~ "|" ~ level ~ packagePath ~ "-" ~ "Processed Transaction" ~ "id=" ~ transactionId.! ~ "from RPC"
  )

  private def receivedSyntacticallyInvalid[$: P]: P[String] = P(
    time ~ "|" ~ level ~ packagePath ~ "-" ~ "Received syntactically invalid transaction" ~ "id=" ~ transactionId.! ~ "reasons="
  )

  private def blockHeader[$: P]: P[Unit] =
    P("header=BlockHeader(id=") ~ blockIdId ~ "parentId=" ~ blockIdId ~ CharsWhile(_ != ')') ~ ")"

  private def minted[$: P]: P[String] = P(
    time ~ "|" ~ level ~ packagePath ~ "-" ~ "Minted" ~ blockHeader ~ "body=Body(transactionIds=" ~ seqCount ~ "List(" ~ seqTransactionId.!
  ).map(_._2)

  def processStreamFromRpc[F[_]: Async](s: Stream[F, Byte]): F[List[String]] =
    s.through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .map(line => parse(line, LogParserTest.processedFromRpc(_)))
      .filter(_.isSuccess)
      .map(_.get.value)
      .compile
      .toList

  def receivedSyntacticallyInvalidStream[F[_]: Async](s: Stream[F, Byte]): F[List[String]] =
    s.through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .map(line => parse(line, LogParserTest.receivedSyntacticallyInvalid(_)))
      .filter(_.isSuccess)
      .map(_.get.value)
      .compile
      .toList

  def mintedStream[F[_]: Async](s: Stream[F, Byte]): F[List[String]] =
    s.through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .map(line => parse(line, LogParserTest.minted(_)))
      .filter(_.isSuccess)
      .map(_.get.value)
      .compile
      .toList

}
