package org.sarsamora.policy_iteration.td.value_functions

import java.util.Arrays

import breeze.linalg.{DenseMatrix, DenseVector, inv, pinv, rank}
import breeze.stats.distributions.Uniform
import org.sarsamora.actions.Action
import org.sarsamora.states.State
import org.sarsamora.{convergenceTolerance, randGen, value_functions}
import breeze.stats.regression.LeastSquaresRegressionResult
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance => lapack}

import scala.collection.mutable

/**
  * Specialized linear approximation action-values with the temporal difference update rule
  * @param coefficients Memory back end for the linear coefficients of each action
  */
class LinearApproximationActionValues(coefficients:Map[Action, mutable.HashMap[String, Double]], val tolerance:Double = convergenceTolerance)
  extends value_functions.LinearApproximationActionValues(coefficients)
  with TDUpdate {

  def this(actions:Set[Action], features:Set[String]){
    this(actions.map{
      a =>
        val uniformDist = Uniform(-1, 1)(randGen)
        val map = new mutable.HashMap[String, Double]()
        // TODO Figure out if the bias affects
        (features + "bias").map(f => f -> uniformDist.sample()).foreach(x => map += x)
        features.map(f => f -> uniformDist.sample()).foreach(x => map += x)
        a -> map
    }.toMap)
  }

  /**
    * Eligibility traces
    */
  private var eligibilityTraces:Array[Double] = Array.fill(featureNames.size)(0d)

  /**
    * Performs the Bellman's update with gradient descent
    *
    * @param current Current state and value
    * @param next Next state and value
    * @param reward Observed reward
    * @param rate Learning rate (alpha)
    * @param decay Expected return decay (gamma)
    * @param lambda Lambda parameter for the eligibility traces (i.e. SARSA(Lambda))
    * @return Convergence status
    */
  override def tdUpdate(current:(State, Action), next:(State, Action),
                        reward: Double, rate: Double,
                        decay: Double, lambda: Double): Boolean = {

    var change = false

    val currentState = current._1
    val currentAction = current._2
    val gradient = valuesToArray(currentState.toFeatures)
    val delta = reward + decay*this(next) - this(current)



    val actionCoefficients = DenseVector(coefficientArrays(currentAction))
    val oldVaues = actionCoefficients.copy
    val newValues = oldVaues + rate*delta*gradient


    val difference = newValues - oldVaues
    val norm = Math.sqrt(difference.t * difference)

    coefficientArrays(currentAction) = newValues.toArray

    if(norm > tolerance)
      change = true

    change
  }


  def lsUpdate(phi:DenseMatrix[Double], pPhi:DenseMatrix[Double], rewards:DenseVector[Double], gamma:Double):Boolean = {

    // Build the matrices to do a least-squares approximation of the solution to Aw = b
    val A = phi.t * (phi - gamma*pPhi)
    val b = phi.t * rewards



    //val lsResult = doLeastSquares(A, b)

    // This updates all the actions' coefficients
//    val coefficients = lsResult.coefficients
//    val r = rank(A)
    val mpinv = A\b
    val coefficients = mpinv //inv(A.t*A)*A.t*b


    // See if the solution changed
    val oldCoefficients = new DenseVector[Double](sortedActions.flatMap(k => coefficientArrays(k)).toArray)
    val difference = oldCoefficients - coefficients
    val norm = Math.sqrt(difference.t * difference)
    println(s"Norm: $norm")
    val changed = if(norm  > convergenceTolerance) true else false

    // Unroll the coefficients into their arrays
    val stride = coefficientArrays.head._2.length
    for((action, ix) <- sortedActions.zipWithIndex){
      val offset = ix*stride
      val slice = coefficients.toArray.slice(offset, offset+stride)
      coefficientArrays(action) = slice
    }

    changed
  }

  // TODO: Put on a better place
  def doLeastSquares(data: DenseMatrix[Double], outputs: DenseVector[Double]): LeastSquaresRegressionResult = {
    val workArray = new Array[Double](2*data.rows*data.cols)
    require(data.rows == outputs.size)
    //require(data.rows > data.cols+1)
    require(workArray.length >= 2*data.rows*data.cols)

    val info = new intW(0)
    lapack.dgels("N", data.rows, data.cols, 1, data.data, data.rows, outputs.data, data.rows, workArray, workArray.length, info)
    if (info.`val` < 0) {
      throw new ArithmeticException("Least squares did not converge.")
    }

    val coefficients = new DenseVector[Double](Arrays.copyOf(outputs.data, data.cols))
    var r2 = 0.0
    for (i <- 0 until (data.rows - data.cols)) {
      r2 = r2 + math.pow(outputs.data(data.cols+i), 2)
    }
    LeastSquaresRegressionResult(coefficients, r2)
  }
}
