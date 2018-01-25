package org.sarsamora.gym.observation_spaces

import org.sarsamora.states.State

case class Discrete(currentState:Int, numStates:Int) extends State {

  override def toFeatures: Map[String, Double] = {
    val states = 0 until numStates

    states.map{
      s =>
        val atState = if(currentState == s) 1.0 else 0.0
        s"state_$s" -> atState
    }.toMap
  }

}
