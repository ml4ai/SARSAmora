package org.sarsamora.gym.CliffWalking.exec

import org.sarsamora.Decays
import org.sarsamora.actions.Action
import org.sarsamora.gym.CliffWalking._
import org.sarsamora.gym.observation_spaces._
import org.sarsamora.policies.TabularValues
import org.sarsamora.policies.{EpGreedyPolicy, LinearApproximationValues}
import org.sarsamora.policy_iteration.td.{EpisodeObservation, EpisodeObserver, IterationObservation, SARSA}

object Train extends App{
  // Hyper parameters
  val numEpisodes = 20000
  val burnInEpisodes = 0
  val learningRate = 1.0
  val decay =   1.0
  ///////////////////

  // Environment instantiation
  val environment = new CliffWalkingEnvironment()
  val activeActions:Set[Action] = Set(Up(), Down(), Left(), Right())
  ////////////////////////////

  // Value function type: Tabular, linear approximation, etc.
  val qFunction = new LinearApproximationValues(activeActions, new Discrete(0, environment.cardinality).toFeatures.keySet)
  //val qFunction = new TabularValues()

  // Exploration parameter for epsilon-greedy policies
  val epsilon = 0.1
  val epsilons = Decays.linearDecay(epsilon, 0.0, numEpisodes, 0).iterator

  // Epsilon greedy policy to iterate, takes as parameters the explorations and the value function instance
  val initialPolicy = new EpGreedyPolicy(epsilon, qFunction)
  /////////////////


  // Learning algorithm
  val alphas = Decays.exponentialDecay(learningRate, 0.01, numEpisodes, 0).iterator
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
  val (learntPolicy, convergenceStatus) = policyIteration.iteratePolicy(initialPolicy)

  // Serialize the learnt policy to a json file
  learntPolicy.save(s"$environment.json")
}
