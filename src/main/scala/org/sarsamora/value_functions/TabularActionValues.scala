package org.sarsamora.value_functions

import breeze.stats.distributions.Uniform
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.sarsamora.actions.Action
import org.sarsamora.randGen
import org.sarsamora.states.{State, StateParser}

import scala.collection.mutable

/**
  * Tabular representation of the Action-Value function
  * @param epsilon epsilon parameter for random initialization of the action-values
  */
class TabularActionValues(epsilon:Double = 1e-2) extends ActionValues {
  // Backend memory structure of the function
  var backEnd = new mutable.HashMap[(State, Action), Double]
  // Distribution to sample the initial vaules
  val uniformDist: Uniform = Uniform(-epsilon, epsilon)(randGen)

  /**
    * Creates an instance out of a set of deserialized values
    * @param loadedBackend Map with the action values loaded by an ActionValue loader
    * @param parser StateParser to convert the String keys into State kets
    * @return
    */
  def this(loadedBackend:collection.Map[Action, collection.Map[String, Double]], parser:StateParser) = {
    this()
    this.backEnd ++= loadedBackend.flatMap{
      case (a, m) =>
        m.map{
          case(s, v) =>
            val state:State = parser.fromString(s)
            (state, a) -> v
        }
    }
  }

  /**
    * Evaluates the action-value for the given state-action pair
    * @param key State-Action pair to be evaluated
    * @return The value of the input pair
    */
  override def apply(key:(State, Action)): Double = {
    // If the backend memory contains the argument, return it
    if(backEnd.contains(key)) {
      backEnd(key)
    }
    // Otherwise initialize it randomly, and return it
    else{
      val v = uniformDist.sample()
      backEnd += key -> v
      v
    }
  }

  /**
    * Converts the function values to a Json object for serialization
    * @return JObject with the current values
    */
  override def toJson: JObject = {
    val x:Map[String, Option[Map[String, Double]]] = backEnd.groupBy{
      case (k, v) => k._2
    }.map{
      case (action, m) =>
        // Map[Action, Option[Map[String, Double]]
        action.toString -> Some(m.map{
          case ((state, _), v) => state.toString -> v
        }.toMap)
    }

    // Create the JObject with the appropriate structure
    ("type" -> "tabular") ~
      ("coefficients" -> x)
  }

  /**
    * Creates a StateValue representation of the action-values by marginalizing the actions
    * @return Map with the state values
    */
  def toStateValues:Map[State, Double] = {
    this.backEnd.groupBy{
      case((s, a), v) => s
    }.mapValues{
      y =>
       y.values.sum
    }
  }

}
