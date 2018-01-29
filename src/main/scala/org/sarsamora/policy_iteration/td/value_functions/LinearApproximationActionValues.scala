package org.sarsamora.policy_iteration.td.value_functions

import breeze.linalg.DenseVector
import org.sarsamora.actions.Action
import org.sarsamora.states.State
import org.sarsamora.{convergenceTolerance, value_functions}

import scala.collection.mutable

/**
  * Specialized linear approximation action-values with the temporal difference update rule
  * @param coefficients Memory back end for the linear coefficients of each action
  */
class LinearApproximationActionValues(coefficients:Map[Action, mutable.HashMap[String, Double]], val tolerance:Double = convergenceTolerance)
  extends value_functions.LinearApproximationActionValues(coefficients)
  with TDUpdate {

  /**
    * Elegibility traces
    */
  private var elegibilityTraces:Array[Double] = Array.fill(featureNames.size)(0d)

  /**
    * Performs the Bellman's update with gradient descent
    * @param current Current state and value
    * @param next Next state and value
    * @param reward Observed reward
    * @param rate Learning rate (alpha)
    * @param decay Expected return decay (gamma)
    * @param lambda Lambda parameter for the elegibility traces (i.e. SARSA(Lambda))
    * @return Convergence status
    */
  override def tdUpdate(current:(State, Action), next:(State, Action),
                        reward: Double, rate: Double,
                        decay: Double, lambda: Double): Boolean = {

    val currentAction = current._2
    val currentState = current._1

    val actionCoefficients = DenseVector(coefficientArrays(currentAction).toArray)

    // The gradient are the feature values because this is a linear function optimizing MSE
    val gradient = valuesToArray(currentState.toFeatures)

    val currentVal = this(current)
    val nextVal = this(next)

    val delta = rate*(reward + decay*nextVal - currentVal)
    elegibilityTraces = (decay*lambda*DenseVector(elegibilityTraces) + gradient).toArray


    var change = false


    val oldActionCoefficients = actionCoefficients
    val newActionCoefficients= oldActionCoefficients + delta*DenseVector(elegibilityTraces)

    coefficientArrays.update(currentAction, new mutable.ArrayBuffer[Double] ++ newActionCoefficients.toArray)

    val d = newActionCoefficients - oldActionCoefficients
    val norm = Math.sqrt(d.t * d)
    if(norm > tolerance)
      change = true

    change
  }
}
