package com.openai.gym.observation_spaces

import org.sarsamora.states.{State, StateParser}

case class Discrete(currentState:Int, numStates:Int) extends State {

  override def toFeatures: Map[String, Double] = {
    val states = 0 until numStates

    states.map{
      s =>
        val atState = if(currentState == s) 1.0 else 0.0
        s"Discrete($s,$numStates)" -> atState
    }.toMap
  }

}

object Discrete extends StateParser {
  override def fromString(description: String): State = {
    val discreteState = raw"Discrete\((\d+),(\d+)\)".r

    description match {
      case discreteState(currentState, numStates) =>
        Discrete(currentState.toInt, numStates.toInt)
      case _ =>
        throw new Exception(s"State $description can not be parsed")
    }
  }
}
