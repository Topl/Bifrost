package co.topl.blockchain.network

import cats.Applicative
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp}
import fs2.concurrent.Topic
import fs2.{Stream, text}
import fs2._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.Temporal._
import cats.effect.IO

import scala.concurrent.duration._
import cats.effect.unsafe.implicits.global
import cats.effect.kernel.GenTemporal._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

object Sandbox extends IOApp.Simple {
  def sharedTopicStream[F[_]: Concurrent](topicId: String)(implicit ec: ExecutionContext): Stream[F, Topic[F, Int]] =
    Stream.eval(Topic[F, Int])

  def addPublisher[F[_] : Temporal: Applicative](topic: Topic[F, Int], fromValue: Int, toValue: Int): Stream[F, Unit] = {
    val sleep = Stream.sleep_[F](FiniteDuration(1, "s"))
    val emit = Stream.emits(fromValue to toValue).covary[F].evalTap[F, Unit]{value: Int =>(println(s"emit $value").pure[F])}./*repeat.*/through(topic.publish)
    emit//.merge(sleep)
  }

  def addSubscriber[F[_]](topic: Topic[F, Int]): Stream[F, Int] =
    topic
      .subscribe(10)
      //.take(4)

  // a request that adds a publisher to the topic
  def requestAddPublisher[F[_]: Temporal](from: Int, to: Int, topic: Topic[F, Int]): Stream[F, Unit] =
    addPublisher(topic, from, to)

  // a request that adds a subscriber to the topic
  def requestAddSubscriber[F[_]](topic: Topic[F, Int], id: String): Stream[F, Unit] =
    addSubscriber(topic).map{value => println(s"$id: $value")}

  def run: IO[Unit] = {
    import ExecutionContext.Implicits.global
    // we simulate  requests that work on a common topic.
    val sharedTopic = sharedTopicStream[IO]("sharedTopic")

    // sharedTopic is passed to your Services, which use it as necessary
    val fr = sharedTopic.flatMap { topic =>
      requestAddPublisher(10, 206, topic) concurrently
        requestAddPublisher(1000, 1020, topic) concurrently
        requestAddSubscriber(topic, "id1") concurrently
        requestAddSubscriber(topic, "id2")
    }

    fr.merge(Stream.sleep_[IO](FiniteDuration(15, "s"))).compile.drain
  }


  def run2: IO[Unit] = {
    val fr = Stream.emits(1 to 100).covary[IO].evalTap{value: Int =>println(s"emit $value").pure[IO]}
    val fr2 = Stream.emits(-1000 to 0).covary[IO].evalTap{value: Int =>println(s"emit $value").pure[IO]}

    (fr2 concurrently fr).compile.drain
  }

}
