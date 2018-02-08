package org.sarsamora.chain_walk

import breeze.stats.distributions.Bernoulli
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.randGen

class ChainWalkingEnvironment  extends Environment {

  private var currentState = ChainWalkingState(1)
  private val bernoulli = new Bernoulli(0.9)(randGen)
  private val rewards = Array[Double](0,1,1,0)

  /**
    * Sequence of actions that can be taken given the current state of the environment
    *
    * @return the sequence of actions
    */
  override def possibleActions = Seq(Left, Right)

  /**
    * Execute the specified action in the environment
    *
    * @param action  Action to execute
    * @param persist Whether to persist the changes in the environment, If false, it would be equivalent to peek
    *                the effects of taking an action before doing it
    * @return Observed reward
    */
override def execute(action: Action, persist: Boolean) = {
  val succeeded = bernoulli.sample()

  val trueAction = action match {
    case Left => if(succeeded) Left else Right
    case Right => if(succeeded) Right else Left
  }

  val outcome = (trueAction, currentState) match {
    case (Right, ChainWalkingState(id)) =>
      if(id < 4)
        id + 1
      else
        id
    case (Left, ChainWalkingState(id)) =>
      if(id > 1)
        id - 1
      else
        id
  }

  currentState = ChainWalkingState(outcome)
  rewards(outcome-1)
}

  /**
    * Current state of the environment
    *
    * @return Object representing the state
    */
  override def observeState = currentState

  /**
    * Whether the current episode within the environment has finished
    *
    * @return
    */
  override def finishedEpisode = false
}
