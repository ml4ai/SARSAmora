package org.sarsamora.environment

import org.sarsamora.actions.Action
import org.sarsamora.states.State

/**
  * Created by enrique on 31/03/17.
  */

/**
  * Interface for an environment
  */
trait Environment {
  /**
    * Sequence of actions that can be taken given the current state of the environment
    * @return the sequence of actions
    */
  def possibleActions: Seq[Action]

  /**
    * Execute the specified action in the environment
    * @param action Action to execute
    * @param persist Whether to persist the changes in the environment, If false, it would be equivalent to peek
    *                the effects of taking an action before doing it
    * @return Observed reward
    */
  def execute(action:Action, persist:Boolean = true):Double

  /**
    * Current state of the environment
    * @return Object representing the state
    */
  def observeState:State

  /**
    * Observe a sequence of alternative current states. They can be used as a function of an action in pair with it.
    * The default implementation if to return only the current state.
    *
    * @return Sequence of alternative current states
    */
  def observeStates:Seq[State] = Seq(observeState)

  // Whether the current episode within the environment has finished
  def finishedEpisode:Boolean
}
