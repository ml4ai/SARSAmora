package org.sarsamora.value_functions

import breeze.linalg.DenseVector
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.sarsamora.actions.Action
import org.sarsamora.states.State

import scala.collection.mutable

/**
  * Linear approximation of the Action-Value function, also known as Q-function
  * @param coefficients Memory back end for the linear coefficients of each action
  */
class LinearApproximationActionValues(coefficients:Map[Action, collection.Map[String, Double]]) extends ActionValues{

  val featureNames:Seq[String] = coefficients.head._2.keySet.toSeq.sorted
  val reverseIndices:Map[Int, String] = featureNames.zipWithIndex.map{ case(f, ix) => ix -> f}.toMap// ++ Map(0 -> "bias")
  val coefficientArrays:mutable.HashMap[Action, Array[Double]] = new  mutable.HashMap[Action, Array[Double]]()

   coefficientArrays ++= coefficients.map{
    case(k, v) =>
      val buff = new mutable.ArrayBuffer[Double]()
      featureNames map v foreach (i => buff += i)
      k -> buff.toArray
  }

  protected def valuesToArray(values:Map[String, Double]):DenseVector[Double] = {
    val v:Seq[Double] = featureNames.indices map {
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

    // Perform the inner product
    DenseVector(actionCoefficients).t * features
  }

  override def toJson: JObject = {
    val maps = (Map()++ coefficientArrays).map {
      case (k, v) =>

        val values = v.zipWithIndex.map {
          case (x, ix) =>
            reverseIndices(ix) -> x
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
