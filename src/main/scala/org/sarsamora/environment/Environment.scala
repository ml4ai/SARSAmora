package org.sarsamora.environment

import org.sarsamora.actions.Action
import org.sarsamora.states.State

/**
  * Created by enrique on 31/03/17.
  */
trait Environment {
  def possibleActions(): Seq[Action]
  def executePolicy(action:Action, persist:Boolean = true):Double
  def observeState:State
  def observeStates:Seq[State]
  def finishedEpisode:Boolean
}
