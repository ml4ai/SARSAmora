package org.sarsamora.gym.FrozenLake.exec

import org.sarsamora.actions.Action
import org.sarsamora.gym.FrozenLake.{Down, FrozenLakeEnvironment, Left, Right, Up}
import org.sarsamora.gym.observation_spaces.Discrete
import org.sarsamora.policies.{EpGreedyPolicy, LinearApproximationValues}
import org.sarsamora.policy_iteration.td.{EpisodeObservation, EpisodeObserver, IterationObservation, SARSA}

/**
  * Learn a FrozenLake policy
  */
object Train extends App{

  // Hyper parameters
  val numEpisodes = 5000
  val burnInEpisodes = 100
  val learningRate = 1.0
  val decay = 1.0
  ///////////////////

  // Environment instantiation
  val environment = new FrozenLakeEnvironment("4x4", true)
  val activeActions:Set[Action] = Set(Up(), Down(), Left(), Right())
  ////////////////////////////

  // Value function type: Tabular, linear approximation, etc.
  val qFunction = new LinearApproximationValues(activeActions, new Discrete(0, environment.cardinality).toFeatures.keySet)
  //val qFunction = new TabularValues()

  // Exploration parameter for epsilon-greedy policies
  val epsilon = 0.2

  // Epsilon greedy policy to iterate, takes as parameters the explorations and the value function instance
  val initialPolicy = new EpGreedyPolicy(epsilon, qFunction)
  /////////////////


  // Learning algorithm
  val policyIteration = new SARSA(
    // Anonymous episode fabric. Each time an episode finished, this is called to return a fresn environment
    // to start the next episode
    () => {
      environment.reset()
      Some(environment)
    }
    , numEpisodes, burnInEpisodes, learningRate, decay)
  /////////////////////


  // Anonymous episode observer. Implements an observer pattern
  // with callbacks for iteration finished and episode finished
  val iterationObserver = new EpisodeObserver {

    /**
    * Raised when an iteration finishes
    *
    * @param data about the current iteration
    */
    override def observeIteration(data: IterationObservation): Unit = {
      // Gives feedback of the current iteration
      println(s"Iteration ${data.iterationNumber}")
    }

      /**
        * Raised when the episode finishes
        *
        * @param data about the episode at its end
        */
      override def episodeFinished(data: EpisodeObservation): Unit = {
        // Prints a nice message if the procedure converged
        if(data.lastEpisode && data.converged){
          println(s"Learning converged!!")
        }
      }
    }
  ////////////////////////////////////////////////////////////

  // Run the learning algorithm. Observe how it also returns the convergence status as well as the iterated policy
  val (learntPolicy, convergenceStatus) = policyIteration.iteratePolicy(initialPolicy, Some(iterationObserver))

  // Serialize the learnt policy to a json file
  learntPolicy.save(s"$environment.json")
}
