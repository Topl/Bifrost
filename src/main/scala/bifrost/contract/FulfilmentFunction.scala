package bifrost.contract

import breeze.interpolation.LinearInterpolator
import breeze.linalg.DenseVector
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

/**
  * Fulfillment is a function that relates time to percentage amount delivered
  */
abstract class FulfilmentFunction {

  def points: Seq[(Long, Double)]

  // timestamp values must be distinct
  require(points.map(_._1).distinct.size == points.size)
  val functionType = "FulfilmentFunction"
  def evaluate(timestamp: Long): Double

  lazy val json: Json = Map(
    "functionType" -> Json.fromString(functionType),
    "points" -> Json.arr(
      points.map(p => Json.arr(Json.fromLong(p._1), Json.fromDouble(p._2).get)):_*
    )
  ).asJson
}

object FulfilmentFunction {
  implicit val decodeFulfilment: Decoder[FulfilmentFunction] = (c: HCursor) => for {
    functionType <- c.downField("functionType").as[String]
    points <- c.downField("points").as[Seq[(Long, Double)]]
  } yield {
    functionType match {
      case "PiecewiseLinearSingle" => PiecewiseLinearSingle(points)
    }
  }
}

/**
  * This will try to interpolate between points
  */
case class PiecewiseLinearSingle(points: Seq[(Long, Double)]) extends FulfilmentFunction {

  override val functionType = "PiecewiseLinearSingle"

  val inputs: DenseVector[Double] = DenseVector[Double](points.map(_._1.toDouble):_*)
  val outputs: DenseVector[Double] = DenseVector[Double](points.map(_._2):_*)
  val f: LinearInterpolator[Double] = LinearInterpolator[Double](inputs, outputs)

  def evaluate(timestamp: Long): Double = {
    f(timestamp.toDouble)
  }
}