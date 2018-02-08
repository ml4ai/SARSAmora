package org.sarsamrora.cliff_walking.exec

import breeze.linalg._
import breeze.stats.distributions.{Multinomial, RandBasis}
import org.sarsamora.{Decays, randGen}
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.policies.EpGreedyPolicy
import org.sarsamora.policy_iteration.mc.MonteCarlo
import org.sarsamora.policy_iteration.td.{LSPI, QLearning, SARSA}
import org.sarsamora.policy_iteration.td.value_functions.LinearApproximationActionValues
//import org.sarsamora.policy_iteration.td.value_functions.TabularActionValues
import org.sarsamora.policy_iteration.td.value_functions.TabularActionValues
import org.sarsamrora.cliff_walking.{CliffWalkingEnvironment, CliffWalkingState, Down, Left, Right, Up}
//import org.sarsamora.policy_iteration.mc.value_functions.TabularActionValues
import org.sarsamora.policy_iteration.{EpisodeObservation, EpisodeObserver, IterationObservation}
//import org.sarsamora.value_functions.{LinearApproximationActionValues, TabularActionValues}

object Train extends App{
  // Hyper parameters
  val numEpisodes = 1000
  val burnInEpisodes = 0
  val learningRate = .1
  val decay =   0.8
  val lambda = 1.0
  ///////////////////

  // Environment instantiation
  val activeActions:Set[Action] = Set(Down, Up, Left, Right)
  ////////////////////////////

  // Value function type: Tabular, linear approximation, etc.
  val qFunction = new LinearApproximationActionValues(activeActions, CliffWalkingState(0).toFeatures.keySet, false)
  //val qFunction = new TabularActionValues()

  // Exploration parameter for epsilon-greedy policies
  val epsilon = 0.1
  val epsilons = Decays.exponentialDecay(epsilon, 0.001, 200000, 10000).iterator

  // Epsilon greedy policy to iterate, takes as parameters the explorations and the value function instance
  val initialPolicy = new EpGreedyPolicy(epsilon, qFunction)
  /////////////////


  // Do a random sample from a multinomial distribution using probs as parameter
  implicit val rand: RandBasis = randGen // This sets the random number generator for the
  val probs = Array.fill[Double](46)(1/46d)
  val dist = new Multinomial(DenseVector(probs))
  val startingPoints = 0 to 46

  val fabric = () => {
    val ix = dist.sample
    val startingPoint = startingPoints(ix)
    Some(new CliffWalkingEnvironment())
  }

  // Learning algorithm
  val learningRates = Decays.exponentialDecay(learningRate, 0.0001, numEpisodes, 0).iterator
  val policyIteration = new QLearning(fabric, numEpisodes, burnInEpisodes, learningRate, decay, lambda)
  //val policyIteration = new MonteCarlo(fabric, numEpisodes, burnInEpisodes, false)


  /////////////////////

  // Anonymous episode observer. Implements an observer pattern
  // with callbacks for iteration finished and episode finished
  val iterationObserver = new EpisodeObserver {

    var successes = 0

    /**
      * Raised when an iteration finishes
      *
      * @param data about the current iteration
      */
    override def observeIteration(data: IterationObservation): Unit = {
      val env = data.environment.asInstanceOf[CliffWalkingEnvironment]
      //println(s"Iteration: ${data.iterationNumber}")
    }

    /**
      * Raised when the episode finishes
      *
      * @param data about the episode at its end
      */
    override def episodeFinished(data: EpisodeObservation): Unit = {
      // Prints a nice message if the procedure converged
      println(s"Episode ${data.episodeNumber}, Iterations: ${data.totalIterations}")
      val env = data.environment.asInstanceOf[CliffWalkingEnvironment]

      //if

      if(data.lastEpisode && data.converged){
        println(s"Learning converged!!")
      }
    }
  }
  ////////////////////////////////////////////////////////////

  // Run the learning algorithm. Observe how it also returns the convergence status as well as the iterated policy
  val (learntPolicy, convergenceStatus) = policyIteration.iteratePolicy(initialPolicy, Some(iterationObserver))

  // Serialize the learnt policy to a json file
  learntPolicy.save("cliff_walking.json")

}
