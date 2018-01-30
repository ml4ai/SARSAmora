package org.sarsamora.value_functions

import breeze.stats.distributions.Uniform
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.sarsamora.actions.Action
import org.sarsamora.randGen
import org.sarsamora.states.State

import scala.collection.mutable

class TabularActionValues(epsilon:Double = 1e-2) extends ActionValues {
  var backEnd = new mutable.HashMap[(String, Action), Double]
  val uniformDist: Uniform = Uniform(-epsilon, epsilon)(randGen)

  def this(loadedBackend:collection.Map[Action, collection.Map[String, Double]]) = {
    this()
    this.backEnd ++= loadedBackend.flatMap{
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
      val v = uniformDist.sample()
      backEnd += (newKey -> v)
      v
    }
  }

  override def toJson: JObject = {
    val x:Map[String, Option[Map[String, Double]]] = backEnd.groupBy{
      case (k, v) => k._2
    }.map{
      case (action, m) =>
        // Map[Action, Option[Map[String, Double]]
        action.toString -> Some(m.map{
          case ((state, _), v) => state -> v
        }.toMap)
    }

    ("type" -> "tabular") ~
      ("coefficients" -> x)
  }

  def toStateValues:Map[String, Double] = {
    this.backEnd.groupBy{
      case((s, a), v) => s
    }.mapValues{
      y =>
       y.values.sum
    }
  }

}
