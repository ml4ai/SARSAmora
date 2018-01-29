package com.openai.gym.CliffWalking.exec

import java.io._

import com.openai.gym.CliffWalking._
import org.sarsamora.Decays
import org.sarsamora.actions.Action
import org.sarsamora.policies.EpGreedyPolicy
import org.sarsamora.policy_iteration.mc.MonteCarlo
import org.sarsamora.policy_iteration.mc.value_functions.TabularActionValues
import org.sarsamora.policy_iteration.{EpisodeObservation, EpisodeObserver, IterationObservation}

import scala.collection.mutable
//import org.sarsamora.value_functions.{LinearApproximationActionValues, TabularActionValues}

object Train extends App{
  // Hyper parameters
  val numEpisodes = 300000
  val burnInEpisodes = 0
  val learningRate = 1
  val decay =   0.5
  val lambda = 1.0
  ///////////////////

  // Environment instantiation
  val environment = new CliffWalkingEnvironment()
  val activeActions:Set[Action] = Set(Up(), Down(), Left(), Right())
  ////////////////////////////

  // Value function type: Tabular, linear approximation, etc.
  //val qFunction = new LinearApproximationActionValues(activeActions, new Discrete(0, environment.cardinality).toFeatures.keySet)
  val qFunction = new TabularActionValues()

  // Exploration parameter for epsilon-greedy policies
  val epsilon = 0.03
  //val epsilons = Decays.linearDecay(epsilon, 0.001, numEpisodes, 0).iterator

  // Epsilon greedy policy to iterate, takes as parameters the explorations and the value function instance
  val initialPolicy = new EpGreedyPolicy(epsilon, qFunction)
  /////////////////


  // Learning algorithm
  val alphas = Decays.exponentialDecay(learningRate, 0.001, numEpisodes, 0).iterator
//  val policyIteration = new SARSA(
//    // Anonymous episode fabric. Each time an episode finished, this is called to return a fresn environment
//    // to start the next episode
//    () => {
//      environment.reset()
//      Some(environment)
//    }
//    , numEpisodes, burnInEpisodes, learningRate, decay, lambda)

  var episodeNumber = 0
  val policyIteration = new MonteCarlo(
    () => {
      episodeNumber += 1
      environment.reset()
      Some(environment)
    }, numEpisodes, burnInEpisodes, true
  )
  /////////////////////


  val coefficientsMemory:Map[Action, Seq[Array[Double]]] = environment.possibleActions.map{
    action =>
      action -> new mutable.ListBuffer[Array[Double]]
  }.toMap

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
//      println(s"Iteration ${data.iterationNumber}")
//      val env = data.environment.asInstanceOf[CliffWalkingEnvironment]
//      env.render()
//      if(data.iterationNumber % 1 == 100) {
//        val values = initialPolicy.values.asInstanceOf[LinearApproximationActionValues]
//
//
//        for ((action, coefficients) <- values.coefficientArrays) {
//          val buffer = coefficientsMemory(action).asInstanceOf[mutable.ListBuffer[Array[Double]]]
//          buffer += coefficients.toArray.clone()
//        }
//      }

    }

    /**
      * Raised when the episode finishes
      *
      * @param data about the episode at its end
      */
    override def episodeFinished(data: EpisodeObservation): Unit = {
      // Prints a nice message if the procedure converged
      println(s"Episode $episodeNumber")
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

  def saveCoefficients(coeff:Map[Action, Seq[Array[Double]]]): Unit = {
    for((action, memory) <- coeff){
      val lines = memory.map{
        a =>
          val l = a.mkString("\t")
          s"$l\n"
      }
      val file = new BufferedWriter(new FileWriter(s"$action.tsv"))
      lines foreach file.write
      file.close()
    }
  }

  saveCoefficients(coefficientsMemory)

}
