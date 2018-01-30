package org.sarsamora.policy_iteration.td.value_functions

import breeze.linalg.DenseVector
import breeze.stats.distributions.Uniform
import org.sarsamora.actions.Action
import org.sarsamora.states.State
import org.sarsamora.{convergenceTolerance, randGen, value_functions}

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

//    val currentAction = current._2
//    val currentState = current._1
//
//    val actionCoefficients = DenseVector(coefficientArrays(currentAction).toArray)
//
//    // The gradient are the feature values because this is a linear function optimizing MSE
//    val gradient = valuesToArray(currentState.toFeatures)
//
//    val currentVal = this(current)
//    val nextVal = this(next)
//
//    val delta = rate*(reward + decay*nextVal - currentVal)
//    eligibilityTraces = (decay*lambda*DenseVector(eligibilityTraces) + gradient).toArray
//
//
//    var change = false
//
//
//    val oldActionCoefficients = actionCoefficients
//    val newActionCoefficients= oldActionCoefficients + delta*DenseVector(eligibilityTraces)
//
//    coefficientArrays.update(currentAction, new mutable.ArrayBuffer[Double] ++ newActionCoefficients.toArray)
//
//    val d = newActionCoefficients - oldActionCoefficients
//    val norm = Math.sqrt(d.t * d)
//    if(norm > tolerance)
//      change = true
//
//    change

    // Update the eligibility traces
//    for(i <- eligibilityTraces.indices){
//      eligibilityTraces(i)  = decay*lambda*eligibilityTraces(i)
//    }

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
}
