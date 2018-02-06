package org.sarsamora.policy_iteration

import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.states.State

/**
  * Trait to be implemented by an observed to the policy iteration process
  */
trait EpisodeObserver {
  /**
    * Raised when an iteration finishes
    * @param data about the current iteration
    */
  def observeIteration(data:IterationObservation)

  /**
    * Raised when the episode finishes
    * @param data about the episode at its end
    */
  def episodeFinished(data:EpisodeObservation)
}



/**
  * Data of the current iteration. Field names are self-describing
  * @param environment Reference to the environment used
  * @param iterationNumber Episode's iteration number
  * @param alpha Learning rate
  * @param gamma Discount factor
  * @param currentState Sarsa
  * @param currentAction sArsa
  * @param reward saRsa
  * @param nextState sarSa
  * @param nextAction sarsA
  */
case class IterationObservation(
                                 environment:Environment,
                                 iterationNumber:Int,
                                 alpha:Double, // Learning rate
                                 gamma:Double, // Discount factor
                                 currentState:State,
                                 currentAction:Action,
                                 reward:Double,
                                 nextState:State,
                                 nextAction:Action
                               )


/**
  * Data at the end of the episode. Field names are self-describing
  * @param environment Reference to the environment used
  * @param totalIterations Number of iterations for this episode
  * @param lastEpisode Was this the last episode?
  * @param converged Did it converged?
  */
case class EpisodeObservation(
                               environment: Environment,
                               totalIterations: Int,
                               episodeNumber:Int,
                               lastEpisode: Boolean,
                               converged:Boolean
                             )