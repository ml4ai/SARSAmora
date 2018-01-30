package org.sarsamora.policy_iteration.mc

import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.policies.EpGreedyPolicy
import org.sarsamora.policy_iteration.mc.value_functions.MCUpdate
import org.sarsamora.policy_iteration.{EpisodeObservation, EpisodeObserver, IterationObservation, PolicyIterator}
import org.sarsamora.states.State

import scala.collection.mutable

/**
  * Policy iteration with MonteCarlo sampling of the episodes
  */
class MonteCarlo(environmentFabric:() => Option[Environment], episodeBound:Int, burnInEpisodes:Int,
                 firstVisit:Boolean = true, maxThreads:Int = 1, tolerance: Double = org.sarsamora.convergenceTolerance)
  extends PolicyIterator(environmentFabric, episodeBound, burnInEpisodes)
  {

  /**
    * Policy iteration algorithm using MC
    *
    * @param policy          Policy to work with
    * @param episodeObserver Optional callback function receiving information each iteration
    * @return Learnt policy and convergence status
    */
  override def iteratePolicy(policy: EpGreedyPolicy, episodeObserver: Option[EpisodeObserver]): (EpGreedyPolicy, Boolean) = {

    // Fetch the initial environment/episode
    var episode = environmentFabric()

    do {

      stable = true

      // Local control variable
      var iterationCounter = 0

      episode match {
        // If there's an environment left to run this episode ...
        case Some(environment) =>

          // State and action memories
          val stateMemory = new mutable.ListBuffer[State]()
          val actionMemory = new mutable.ListBuffer[Action]()
          val rewardMemory = new mutable.ListBuffer[Double]()

          var currentState = environment.observeState
          var currentAction = policy.selectAction(currentState, environment.possibleActions)

          // Repeat while the episode finishes
          while(!environment.finishedEpisode){

            // Increase the iteration counter
            iterationCounter += 1

            // Execute the action chosen by the policy
            val reward = environment.execute(currentAction)
            // Store the observations
            stateMemory += currentState
            actionMemory += currentAction
            rewardMemory += reward

            // Observe the next state and action
            val nextState = environment.observeState
            val nextAction = policy.selectAction(nextState, environment.possibleActions)

            val prevState = currentState
            val prevAction = currentAction

            currentState = nextState
            currentAction = nextAction

            // Call the observed if specified
            episodeObserver match {
              case Some(observer) =>
                // Fill the observation
                val observation = IterationObservation(episode.get, iterationCounter, 0, 0, prevState, prevAction, reward, nextState, nextAction)
                // Pass to observer
                observer.observeIteration(observation)
              case None => Unit
            }
          }

          // Store the reward of the last observed state and action, which by definition is zero
          //rewardMemory += 0d

          // Do the update
          val actionValues = policy.values match {
            case v:MCUpdate => v
            case _ => throw new IllegalArgumentException("The action-values don't implement MC update")
          }

          // Put together the observations
          val sample = stateMemory zip actionMemory zip rewardMemory map {
            case ((s, a), r) => (s,a,r)
          }

          // Do the update, and if it didn't change the action values, the policy converged
          stable = !actionValues.mcUpdate(sample, firstVisit, tolerance)

        // In case we ran out of environments, we're done
        case None => Unit
      }

      val prevEpisode = episode

      episode = environmentFabric()

      episodeCount += 1
      // Call the observed if specified
      episodeObserver match {
        case Some(observer) =>
          // Fill the observation
          val observation = EpisodeObservation(prevEpisode.get, iterationCounter, episodeCount, episode.isEmpty, stable)
          // Pass to observer
          observer.episodeFinished(observation)
        case None => Unit
      }

    }
    // If the next environment is defined and if we're within the episode bounds and if it's unstable and if we went
    // through the burn in process. Phew ....
    while(episode.isDefined && (!stable || episodeCount <= burnInEpisodes) && episodeCount <= episodeBound)



    if(stable)
      logger.info(s"Converged on $episodeCount episodes")
    else
      logger.info(s"Didn't converge")

    // Return the tuned policy and the convergence status
    (policy, stable)
  }

}
