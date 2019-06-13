package bifrost.program

import breeze.interpolation.LinearInterpolator
import breeze.linalg.{*, DenseMatrix, DenseVector}
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

/**
  * Share function dictates how over delivered goods' value are split up
  * First double is going to be the x-axis number.
  */
abstract class ShareFunction {

  def points: Seq[(Double, (Double, Double, Double))]

  // delivered values must be distinct and sum to 1
  require(points.map(_._1).distinct.size == points.size)

  // anchor at 0
  require(points.exists(_._1 == 0.0))

  // Ensure they all add to 1 and are non-negative
  points.map(_._2).foreach(t => {
    require(Math.abs(t._1 + t._2 + t._3 - 1) < 1e-5)
    require(t._1 >= 0); require(t._2 >= 0); require(t._3 >= 0)
  })

  val functionType = "ShareFunction"
  def evaluate(x: Double): (Double, Double, Double)

  lazy val json: Json = Map(
    "functionType" -> Json.fromString(functionType),
    "points" -> Json.arr(points.map(p =>
      Json.arr(
        Json.fromDouble(p._1).get,
        Json.arr(Json.fromDouble(p._2._1).get, Json.fromDouble(p._2._2).get, Json.fromDouble(p._2._3).get)
      )
    ):_*)
  ).asJson
}

object ShareFunction {
  implicit val decodeShare: Decoder[ShareFunction] = (c: HCursor) => for {
    functionType <- c.downField("functionType").as[String]
    points <- c.downField("points").as[Seq[(Double, (Double, Double, Double))]]
  } yield {
    functionType match {
      case "PiecewiseLinearMultiple" => PiecewiseLinearMultiple(points)
    }
  }
}


/**
  * This will try to interpolate between points
  * @param points: Sequence of
  *               Double: Yield. ex. 0.25 = 25% over delivery
  *               Double: Producer's cut in percentage
  *               Double: Hub's cut in percentage
  *               Double: Investor's cut in percentage
  */
case class PiecewiseLinearMultiple(points: Seq[(Double, (Double, Double, Double))]) extends ShareFunction {

  override val functionType = "PiecewiseLinearMultiple"

  val inputs: DenseVector[Double] = DenseVector[Double](points.map(_._1):_*)
  val outputs: DenseMatrix[Double] = DenseMatrix(points.map(p => DenseVector(p._2._1, p._2._2, p._2._3)):_*).t
  val f: DenseVector[LinearInterpolator[Double]] = outputs(*,::).map(row => LinearInterpolator[Double](inputs, row))

  def evaluate(yieldPercentage: Double): (Double, Double, Double) = {
    val res = f.map(_(yieldPercentage.toDouble))
    (res(0), res(1), res(2))
  }
}