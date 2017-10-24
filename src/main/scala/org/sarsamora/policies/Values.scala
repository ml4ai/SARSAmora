package org.sarsamora.policies

import breeze.linalg.DenseVector
import breeze.stats.distributions.Uniform
import org.sarsamora.randGen
import org.sarsamora.actions._

import collection.mutable
import org.sarsamora.states.State
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

import scala.language.implicitConversions

/**
  * Created by enrique on 26/03/17.
  */


abstract class Values(val tolerance:Double = 3e-4){

  def apply(key:(State, Action)):Double
  def tdUpdate(current:(State, Action), next:(State, Action), reward:Double, rate:Double, decay:Double):Boolean
  def toJson:JObject
}

trait ActionValueLoader{
  def loadActionValues(ast:JObject):Map[Action, mutable.HashMap[String, Double]]
}

object Values{

  def loadValues(ast:JObject, actionValueLoader: ActionValueLoader):Values = {
    (ast \ "type") match {
      case JString("linear") =>

        val coefficientsMap = actionValueLoader.loadActionValues(ast)

        new LinearApproximationValues(coefficientsMap)
      case _ =>
        throw new NotImplementedError("Not yet implemented")
    }
  }
}

class TabularValues(default:Double) extends Values {
  val backEnd = new mutable.HashMap[(State, Action), Double]

  override def apply(key:(State, Action)): Double = {
    if(backEnd.contains(key))
      backEnd(key)
    else{
      backEnd += (key -> default)
      default
    }
  }

  override def tdUpdate(current:(State, Action), next:(State, Action), reward:Double, rate:Double, decay:Double) = {
    val value:Double = this(current)
    val newValue:Double = value + (rate*(reward + decay*this(next) - value))

    backEnd(current) =  newValue

    // Return whether the current changed above the requested tolerance
    if(Math.abs(newValue - value) > tolerance)
      true
    else
      false
  }

  override def toJson = {
    ("coming" -> "soon")
  }

}


class LinearApproximationValues(val coefficients:Map[Action, mutable.HashMap[String, Double]]) extends Values {

  val uniformDist = Uniform(-1, 1)(randGen)

  def this(actions:Set[Action]) = {
    this(actions.map(a => a -> new mutable.HashMap[String, Double]).toMap)
  }

  //val coefficients = new mutable.HashMap[String, Double]

  val coefficientMemory:Map[Action, mutable.ArrayBuffer[DenseVector[Double]]] = coefficients.keys.map{
    k => k -> new mutable.ArrayBuffer[DenseVector[Double]]
  }.toMap

  override def apply(key:(State, Action)): Double = {

    val action = key._2
    val actionCoefficients = coefficients(action)

    // Encode the state vector into features
    val features = Map("bias" -> 1.0) ++ key._1.toFeatures

    // Do the dot product with the coefficients
    val products = features map {
      case (k, v) =>
        val coefficient = actionCoefficients.lift(k).getOrElse(uniformDist.sample())
        coefficient*v
    }

    // Return the summation
    products.sum
  }

  override def tdUpdate(current:(State, Action), next:(State, Action), reward: Double, rate: Double, decay: Double): Boolean = {

    val action = current._2
    val actionCoefficients = coefficients(action)

    // The gradient are the feature values because this is a linear function optimizing MSE
    val gradient = Map("bias" -> 1.0) ++ current._1.toFeatures

    val currentVal = this(current)
    val nextVal =this(next)

    val ret = reward + decay*nextVal
    val delta =rate*(ret-currentVal)

    var change = false

    // Now perform the update
    for(k <- gradient.keys){
      val value = actionCoefficients.lift(k).getOrElse(uniformDist.sample())
      val newValue = value + delta*gradient(k)
      actionCoefficients(k) = newValue

      // Return whether the current changed above the requested tolerance
      if(Math.abs(newValue - value) > tolerance)
        change = true

    }

    storeCurrentCoefficients()

    change
  }

  override def toJson = {
    val maps = coefficients.map{case (k, v) =>

      val values = v.toMap
      if(values.size > 0)
        (k.toString -> Some(values))
      else
        (k.toString -> None)

    }


    ("type" -> "linear") ~
      ("coefficients" -> maps)
  }

  private def storeCurrentCoefficients(): Unit ={
    for(action <- coefficients.keys){
      val localCoefficients = coefficients(action)
      val keys = localCoefficients.keySet.toSeq.sorted
      val vals = keys map localCoefficients
      coefficientMemory(action) += DenseVector(vals.toArray)
    }
  }
}
