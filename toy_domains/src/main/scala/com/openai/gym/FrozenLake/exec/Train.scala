package com.openai.gym.FrozenLake.exec

import com.openai.gym.FrozenLake._
import com.openai.gym.observation_spaces.Discrete
import org.sarsamora.Decays
import org.sarsamora.actions.Action
import org.sarsamora.policies.EpGreedyPolicy
import org.sarsamora.policy_iteration.td.SARSA
import org.sarsamora.policy_iteration.td.value_functions.LinearApproximationActionValues
import org.sarsamora.policy_iteration.{EpisodeObservation, EpisodeObserver, IterationObservation}
//import org.sarsamora.policy_iteration.mc.value_functions.TabularActionValues

/**
  * Learn a FrozenLake policy
  */
object Train extends App{

  // Hyper parameters
  val numEpisodes = 1000
  val burnInEpisodes = 10
  val learningRate = 1d
  val decay = 1d
  val lambda = 1
  ///////////////////

  // Environment instantiation
  val environment = new FrozenLakeEnvironment("4x4", false)
  val activeActions:Set[Action] = Set(Up(), Down(), Left(), Right())
  ////////////////////////////

  // Value function type: Tabular, linear approximation, etc.
  val qFunction = new LinearApproximationActionValues(activeActions, new Discrete(0, environment.cardinality).toFeatures.keySet)
  //val qFunction = new TabularActionValues()

  // Exploration parameter for epsilon-greedy policies
  val epsilon = 0.3
  val epsilons = Decays.exponentialDecay(epsilon, 0.00, numEpisodes, 0).iterator
  // Epsilon greedy policy to iterate, takes as parameters the explorations and the value function instance
  val initialPolicy = new EpGreedyPolicy(epsilon, qFunction)
  /////////////////

  val alphas = Decays.exponentialDecay(learningRate, 0d, numEpisodes, 0).iterator

  // Learning algorithm
  val policyIteration = new SARSA(
    // Anonymous episode fabric. Each time an episode finished, this is called to return a fresn environment
    // to start the next episode
    () => {
      environment.reset()
      Some(environment)
    }
    , numEpisodes, burnInEpisodes, alphas, decay, lambda)

//  val policyIteration = new MonteCarlo(
//    // Anonymous episode fabric. Each time an episode finished, this is called to return a fresn environment
//    // to start the next episode
//    () => {
//      environment.reset()
//      Some(environment)
//    }
//    , numEpisodes, burnInEpisodes, tolerance=1e-3)
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
      //println(s"Iteration ${data.iterationNumber}")
      //environment.render()
    }

      /**
        * Raised when the episode finishes
        *
        * @param data about the episode at its end
        */
      override def episodeFinished(data: EpisodeObservation): Unit = {
        // Prints a nice message if the procedure converged
        println(s"Episode ${data.episodeNumber}, iterations: ${data.totalIterations}")
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
