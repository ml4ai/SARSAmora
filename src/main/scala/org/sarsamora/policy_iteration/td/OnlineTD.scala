package org.sarsamora.policy_iteration.td

import com.typesafe.scalalogging.LazyLogging
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.policies.{EpGreedyPolicy, Policy}
import org.sarsamora.policy_iteration.td.value_functions.TDUpdate
import org.sarsamora.policy_iteration.{EpisodeObservation, EpisodeObserver, IterationObservation}
import org.sarsamora.states.State
import org.sarsamora.value_functions.ActionValues


/**
  * Iterates a policy by doing SARSA
  *
  * Created by enrique on 26/03/17.
  *
  * @param environmentFabric Fabric with multiple environments, each call to it will return one environment
  *                          which encapsulates an episode. There's a 1-to-1 correspondence between
  *                          environments and episode numbers
  * @param episodeBound Upper limit on the number of episodes to iterate through
  * @param burnInEpisodes Minimum amount of episodes to iterate before stopping
  * @param alphas Learning rate stream
  * @param gamma Discount factor
  */
abstract class OnlineTD(environmentFabric:() => Option[Environment],
                        episodeBound:Int, burnInEpisodes:Int,
                        alphas:Iterator[Double],
                        gamma:Double = 0.8, lambda:Double = 1.0) extends LazyLogging {

  // Stability flag controlling convergence
  var stable = true
  // Control variables
  var episodeCount = 0


  /**
    * Selects the next action according to a criteria. For SARSA, use the iterated policy, for QLearning use the greedy
    * choice given the action values.
    *
    * @param nextState State to condition the actions
    * @param possibleActions Set of actions to chose from
    * @param policy Policy being iterated
    * @return Chosen action
    */
  protected def selectNextAction(nextState:State,
                                 possibleActions:Iterable[Action],
                                 policy: Policy,
                                 actionValues:ActionValues): Action

  /**
    * Policy iteration algorithm
    * @param policy Policy to work with
    * @param episodeObserver Optional callback function receiving information each iteration
    * @return Learnt policy and convergence status
    */
  def iteratePolicy(policy:EpGreedyPolicy, episodeObserver:Option[EpisodeObserver] = None):(Policy, Boolean)= {


    // Fetch the initial environment/episode
    var episode = environmentFabric()

    do {

      stable = true

      // Local control variable
      var iterationCounter = 0

      episode match {
        // If there's an environment left to run this episode ...
        case Some(environment) =>

          val currentAlpha = alphas.next

          // Observe the initial state
          var currentState = environment.observeState

          // Evaluate the policy
          var currentAction = policy.selectAction(currentState, environment.possibleActions)



          // Enter into the episode loop
          while(!environment.finishedEpisode){
            // Execute chosen action and observe reward
            val reward = environment.execute(currentAction)

            // Observe the new state after executing the action
            val nextState = environment.observeState

            // Chose the next action for the TD update
            val nextAction4Update = this.selectNextAction(nextState, environment.possibleActions, policy, policy.values)



            // Perform the update
            val actionValues = policy.values match {
              case v:TDUpdate => v
              case _ => throw new IllegalArgumentException("Action values don't implement the TD Update")
            }

            val changed = actionValues.tdUpdate((currentState, currentAction), (nextState, nextAction4Update), reward, currentAlpha, gamma, lambda)

            // Keep track of the fluctuations of the values
            if(changed)
              stable = false

            iterationCounter += 1

            // Chose the next action for control
            val nextAction = policy.selectAction(nextState, environment.possibleActions)

            // Call the observer for this iteration
            episodeObserver match {
              case Some(observer) =>
                // Fill the observation
                val observation = IterationObservation(environment, iterationCounter,
                  currentAlpha, gamma,
                  currentState, currentAction,
                  reward, nextState, nextAction
                )

                // Pass to observer
                observer.observeIteration(observation)
              case None => Unit
            }

            // Update the state and action
            currentState = nextState
            currentAction = nextAction


          }

          // Increment the episode count
          episodeCount += 1

          if(episodeCount % 10 == 0)
            logger.info(s"Episode $episodeCount")

        // In case we ran out of environments, we're done
        case None => Unit
      }

      val prevEpisode = episode
      episode = environmentFabric()
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