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

  // Feature vocabulary sorted lexicographically
  val featureNames:Seq[String] = coefficients.head._2.keySet.toSeq.sorted
  // Inverted index of the feature vocabulary
  val reverseIndices:Map[Int, String] = featureNames.zipWithIndex.map{ case(f, ix) => ix -> f}.toMap
  // Backend of the feature coefficients
  val coefficientArrays:mutable.HashMap[Action, Array[Double]] = new mutable.HashMap[Action, Array[Double]]()


  // Convert the coefficients into arrays for optimization
  coefficientArrays ++= coefficients.map{
    case(k, v) =>
      val buff = new mutable.ArrayBuffer[Double]()
      featureNames map v foreach (i => buff += i)
      k -> buff.toArray
  }

  // Actions sorted lexicographically
  val sortedActions = this.coefficientArrays.keys.toSeq.sortBy(_.toString)

  // TODO: Explain the expanded parameter
  /**
    * Converts a feature dictionary into a breeze vector with a bias term for learning
    * @param values Feature dictionary created by State.toFeatures
    * @param expanded
    * @return DenseVector index with the feature values correctly indexed
    */
   def valuesToArray(values:Map[String, Double], action:Option[Action] = None):DenseVector[Double] = {

     val size = if(action.isDefined) featureNames.size * sortedActions.size else featureNames.size

     val backend:Array[Double] = Array.fill(size)(0)

     featureNames.indices foreach {
       ix => {
         val offset = if (action.isDefined) sortedActions.indexOf(action.get) else 0

         val v = if (reverseIndices(ix) == "bias") {
           // Bias term
           1.0
         }
         else {
           val featureName = reverseIndices(ix)
           values(featureName)
         }

         backend(offset+ix) = v
       }
     }

     new DenseVector[Double](backend)
  }

  /**
    * Computes the linear approximation of the Action-Value function for the state-action pair
    * @param key State-Action pair to be evaluated
    * @return The value of the input pair
    */
  override def apply(key:(State, Action)): Double = {

    // Fetch the correct coefficients
    val action = key._2
    val actionCoefficients = coefficientArrays(action)

    // Encode the state vector into features
    val features = valuesToArray(key._1.toFeatures)

    // Perform the inner product
    DenseVector(actionCoefficients).t * features
  }

  /**
    * Serialize the coefficients to a Json AST
    * @return JObject to be serialized
    */
  override def toJson: JObject = {
    val maps = (Map() ++ coefficientArrays).map {
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

    // Build the JObject with the appropriate structure
    ("type" -> "linear") ~
      ("coefficients" -> maps)
  }

}
