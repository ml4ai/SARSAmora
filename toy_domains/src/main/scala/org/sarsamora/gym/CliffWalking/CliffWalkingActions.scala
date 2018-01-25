package org.sarsamora.gym.CliffWalking

import org.json4s.DefaultFormats
import org.json4s.JsonAST.{JObject, JValue}
import org.sarsamora.actions.Action
import org.sarsamora.policies.ActionValueLoader

import scala.collection.mutable

abstract class CliffWalkingAction(val id:Int) extends Action

case class Up() extends CliffWalkingAction(0)
case class Down() extends CliffWalkingAction(2)
case class Left() extends CliffWalkingAction(3)
case class Right() extends CliffWalkingAction(1)


class CliffWalkingActionsActionValues extends ActionValueLoader {
  implicit lazy val formats = DefaultFormats


  private def extractCoefficients(ast:JValue, name:CliffWalkingAction):Option[(Action, mutable.HashMap[String, Double])] = {
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

  override def loadActionValues(ast:JObject) = {

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