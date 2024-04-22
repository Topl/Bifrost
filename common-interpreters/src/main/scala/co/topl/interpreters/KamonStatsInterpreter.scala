package co.topl.interpreters

import cats.effect.kernel.Sync
import io.circe.Json
import cats.effect.kernel.Ref
import cats.effect.implicits._
import cats.implicits._

import kamon.Kamon
import kamon.metric.Metric
import cats.effect.kernel.Resource
import co.topl.algebras.Stats
import kamon.tag.TagSet
import cats.Applicative

object KamonStatsRef {

  def make[F[_]: Sync]: Resource[F, Stats[F]] =
    for {
      metrics <- Ref.of[F, Map[String, Metric[_, _]]](Map.empty).toResource
    } yield (new Stats[F] {

      def incrementCounter(statName: String, description: String, attributes: Map[String, Json]): F[Unit] =
        for {
          // Create or retrieve the counter from the metrics map by ref.
          counter <- metrics.modify { m =>
            m.get(statName) match {
              case Some(counter) =>
                (m, counter)
              case None =>
                val counter = Kamon.counter(statName, description)
                val m1 = m + (statName -> counter)
                (m1, counter)
            }
          }

          metricTags <- for {
            tagBuilder <- Sync[F].delay(TagSet.builder())
            _ <- Sync[F].delay(attributes.map { case (k, v) =>
              tagBuilder.add(k, v.noSpaces.stripPrefix("\"").stripSuffix("\""))
            })
            tags <- Sync[F].delay(tagBuilder.build())
          } yield (tags)

          _ <- Sync[F].delay(counter.asInstanceOf[Metric.Counter].withTags(metricTags).increment())
        } yield ()

      def decrementCounter(statName: String, description: String, attributes: Map[String, Json]): F[Unit] = ???

      def recordGauge(statName: String, description: String, attributes: Map[String, Json], value: Json): F[Unit] =
        for {
          // Create or retrieve the gauge from the metrics map by ref.
          gauge <- metrics.modify { m =>
            m.get(statName) match {
              case Some(gauge) =>
                (m, gauge)
              case None =>
                val gauge = Kamon.gauge(statName, description)
                val m1 = m + (statName -> gauge)
                (m1, gauge)
            }
          }

          metricTags <- for {
            tagBuilder <- Sync[F].delay(TagSet.builder())
            _ <- Sync[F].delay(attributes.map { case (k, v) =>
              tagBuilder.add(k, v.noSpaces.stripPrefix("\"").stripSuffix("\""))
            })
            tags <- Sync[F].delay(tagBuilder.build())
          } yield (tags)

          _ <- Sync[F].delay(gauge.asInstanceOf[Metric.Gauge].withTags(metricTags).update(value.asNumber.get.toDouble))
        } yield ()

      def recordHistogram(statName: String, description: String, attributes: Map[String, Json], value: Json): F[Unit] =
        for {
          // Create or retrieve the histogram from the metrics map by ref.
          histogram <- metrics.modify { m =>
            m.get(statName) match {
              case Some(histogram) =>
                (m, histogram)
              case None =>
                val histogram = Kamon.histogram(statName, description)
                val m1 = m + (statName -> histogram)
                (m1, histogram)
            }
          }

          metricTags <- for {
            tagBuilder <- Sync[F].delay(TagSet.builder())
            _ <- Sync[F].delay(attributes.map { case (k, v) =>
              tagBuilder.add(k, v.noSpaces.stripPrefix("\"").stripSuffix("\""))
            })
            tags <- Sync[F].delay(tagBuilder.build())
          } yield (tags)

          _ <- Sync[F].delay(
            histogram.asInstanceOf[Metric.Histogram].withTags(metricTags).record(value.asNumber.get.toLong.get)
          )
        } yield ()

        def writeFile(statName: String, data: Json): F[Unit] = Applicative[F].unit
    })
}
