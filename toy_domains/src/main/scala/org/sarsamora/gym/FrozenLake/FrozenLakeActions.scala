package org.sarsamora.gym.FrozenLake

import org.json4s.DefaultFormats
import org.json4s.JsonAST.{JObject, JValue}
import org.sarsamora.actions.Action
import org.sarsamora.value_functions.ActionValueLoader

import scala.collection.mutable

abstract class FrozenLakeAction(val id:Int) extends Action

case class Up() extends FrozenLakeAction(3)
case class Down() extends FrozenLakeAction(1)
case class Left() extends FrozenLakeAction(0)
case class Right() extends FrozenLakeAction(2)


class FrozenLakeActionsActionValues extends ActionValueLoader {
  implicit lazy val formats: DefaultFormats.type = DefaultFormats


  private def extractCoefficients(ast:JValue, name:FrozenLakeAction):Option[(Action, mutable.HashMap[String, Double])] = {
    ast \ name.toString match {
      case JObject(obj) =>
        val coefficients = new mutable.HashMap[String, Double]()

        for((k, v) <- obj){
          coefficients += (k -> v.extract[Double])
        }

        Some(name -> coefficients)

      case _ =>
        None
    }
  }

  override def loadActionValues(ast:JObject): Map[Action, collection.Map[String, Double]] = {

    val coefficients = ast \ "coefficients"
    val up = extractCoefficients(coefficients, Up())
    val down= extractCoefficients(coefficients, Down())
    val left = extractCoefficients(coefficients, Left())
    val right = extractCoefficients(coefficients, Right())

    val coefficientsMap = Seq[Option[(Action, mutable.HashMap[String, Double])]](up, down, left, right).collect{
      case Some((name, coeff)) => name -> coeff
    }.toMap

    coefficientsMap
  }
}