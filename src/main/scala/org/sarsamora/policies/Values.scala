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

      case JString("tabular") =>
        val functionTable = actionValueLoader.loadActionValues(ast)
        // TODO: Do this cleanly!!
        val mutableTable = new mutable.HashMap[Action, mutable.HashMap[String, Double]]()
        mutableTable ++= functionTable
        new TabularValues(mutableTable)

      case _ =>
        throw new NotImplementedError("Not yet implemented")
    }
  }
}

class TabularValues() extends Values {
  var backEnd = new mutable.HashMap[(String, Action), Double]
  val uniformDist = Uniform(-1, 1)(randGen)

  def this(loadedBackend:mutable.HashMap[Action, mutable.HashMap[String, Double]]) = {
    this()
    this.backEnd = loadedBackend.flatMap{
      case (a, m) =>
        m.map{
          case(s, v) =>
            (s, a) -> v
        }
    }
  }

  override def apply(key:(State, Action)): Double = {
    val newKey = (key._1.toString, key._2)
    if(backEnd.contains(newKey))
      backEnd(newKey)
    else{
      val v = 0//uniformDist.sample()
      backEnd += (newKey -> v)
      v
    }
  }

  override def tdUpdate(current:(State, Action), next:(State, Action), reward:Double, rate:Double, decay:Double) = {
    val value:Double = this(current)
    val newValue:Double = value + (rate*(reward + decay*this(next) - value))

    backEnd((current._1.toString(), current._2)) =  newValue

    // Return whether the current changed above the requested tolerance
    if(Math.abs(newValue - value) > tolerance)
      true
    else
      false
  }

  override def toJson = {
    val x:Map[String, Option[Map[String, Double]]] = backEnd.groupBy{
      case (k, v) => k._2
    }.map{
      case (action, m) =>
        // Map[Action, Option[Map[String, Double]]
        action.toString -> Some(m.map{
          case ((state, _), v) => state -> v
        }.toMap)
    }

//    val maps = backEnd.map { case (k, v) =>
//
//      val values = v.toMap
//      if (values.nonEmpty)
//        k.toString -> Some(values)
//      else
//        k.toString -> None
//
//    }


    ("type" -> "tabular") ~
      ("coefficients" -> x)
  }


}




class LinearApproximationValues(coefficients:Map[Action, mutable.HashMap[String, Double]]) extends Values {

  val featureNames:Seq[String] = coefficients.head._2.keySet.toSeq.sorted
  val reverseIndices:Map[Int, String] = featureNames.zipWithIndex.map{ case(f, ix) => ix -> f}.toMap// ++ Map(0 -> "bias")
  val coefficientArrays:mutable.HashMap[Action, mutable.ArrayBuffer[Double]] = new  mutable.HashMap[Action, mutable.ArrayBuffer[Double]]()

   coefficientArrays ++= coefficients.map{
    case(k, v) =>
      val buff = new mutable.ArrayBuffer[Double]()
      featureNames map (v) foreach (i => buff += i)
      k -> buff
  }

  def this(actions:Set[Action], features:Set[String]) = {
    this(actions.map{
      a =>
        val uniformDist = Uniform(-1, 1)(randGen)
        val map = new mutable.HashMap[String, Double]()
        (features + "bias").map(f => f -> uniformDist.sample()).foreach(x => map += x)
        a -> map
    }.toMap)
  }

  private def valuesToArray(values:Map[String, Double]):DenseVector[Double] = {
    val v:Seq[Double] = 0 until featureNames.size map {
      ix =>
        if(reverseIndices(ix) == "bias"){
          // Bias term
          1.0
        }
        else{
          val featureName = reverseIndices(ix)
          values(featureName)
        }
    }

    new DenseVector[Double](v.toArray)
  }

  override def apply(key:(State, Action)): Double = {

    val action = key._2
    val actionCoefficients = coefficientArrays(action)

    // Encode the state vector into features
    val features = valuesToArray(key._1.toFeatures)


    DenseVector(actionCoefficients.toArray).t * features
  }

  override def tdUpdate(current:(State, Action), next:(State, Action), reward: Double, rate: Double, decay: Double): Boolean = {

    val action = current._2
    val actionCoefficients = DenseVector(coefficientArrays(action).toArray)

    // The gradient are the feature values because this is a linear function optimizing MSE
    val gradient = valuesToArray(current._1.toFeatures)

    val currentVal = this(current)
    val nextVal = this(next)

    val ret = reward + decay*nextVal
    val delta =rate*(ret-currentVal)

    var change = false


    val oldActionCoefficients = actionCoefficients
    val newActionCoefficients= oldActionCoefficients + delta*gradient

    val temp = new mutable.ArrayBuffer[Double]
    temp ++= newActionCoefficients.toArray
    coefficientArrays.update(action, temp)

    val d = newActionCoefficients - oldActionCoefficients
    val norm = Math.sqrt(d.t * d)
    if(norm > tolerance)
      change = true

    change
  }

  override def toJson = {
    val maps = (Map()++ coefficientArrays).map {
      case (k, v) =>

        val values = v.toArray.zipWithIndex.map {
          case (v, ix) =>
            reverseIndices(ix) -> v
        }.toMap

        if(values.nonEmpty)
          k.toString -> Some(values)
        else
          k.toString -> None


    }



    ("type" -> "linear") ~
      ("coefficients" -> maps)
  }

}
