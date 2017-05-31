package bifrost.contract

import breeze.interpolation.LinearInterpolator
import breeze.linalg.DenseVector

/**
  * Fulfillment is a function that relates time to percentage amount delivered
  * @param points
  *               a Sequence of (
  *               Long: timestamp for scheduled delivery
  *               Double: Percentage amount of total delivery ex. 0.25
  *               )
  */
abstract class FulfilmentFunction(val points: Seq[(Long, Double)]){

  // timestamp values must be distinct
  require(points.map(_._1).distinct.size == points.size)
  val functionType = "FulfilmentFunction"
  def evaluate(timestamp: Long): Double
}

/**
  * This will try to interpolate between points
  */
class PiecewiseLinearSingle(points: Seq[(Long, Double)]) extends FulfilmentFunction(points) {

  override val functionType = "PiecewiseLinearSingle"

  val inputs: DenseVector[Double] = DenseVector[Double](points.map(_._1.toDouble):_*)
  val outputs: DenseVector[Double] = DenseVector[Double](points.map(_._2):_*)
  val f: LinearInterpolator[Double] = LinearInterpolator[Double](inputs, outputs)

  def evaluate(timestamp: Long): Double = {
    f(timestamp.toDouble)
  }
}