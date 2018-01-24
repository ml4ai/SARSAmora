package org.sarsamora.gym.FrozenLake

import org.sarsamora.Decays
import org.sarsamora.actions.Action
import org.sarsamora.states.State
import org.sarsamora.gym.JepEnvironment
import org.sarsamora.gym.observation_spaces.Discrete
import org.sarsamora.environment.Environment
import org.sarsamora.policies.{EpGreedyPolicy, LinearApproximationValues, TabularValues}
import org.sarsamora.policy_iteration.td._
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.{DynamicObject, Object}

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

class FrozenLakeV0 extends JepEnvironment {

  // Initialize the Gym Environment
  interpeter.eval("from gym.envs.toy_text.frozen_lake import FrozenLakeEnv")
  val env = Object("FrozenLakeEnv(is_slippery = False, map_name='8x8')").asInstanceOf[DynamicObject]
  //val env = gym.make("FrozenLake-v0")
  var currentState:Discrete = Discrete(0, 64)
  var done = false

  env.reset()

  def reset():Unit = {
    env.reset()
    currentState = Discrete(0, 64)
    done = false
  }

  def render():Unit = {
    env.render()
  }

  override def possibleActions() = Seq(Up(), Down(), Left(), Right())

  override def executePolicy(action: Action, persist: Boolean) = {
    val frozenAction = action.asInstanceOf[FrozenLakeAction]
    val x = env.step(frozenAction.id)

    val returnedData = x.value.asInstanceOf[java.util.List[Any]]

    val state = returnedData(0).asInstanceOf[Int]
    currentState = Discrete(state, 64)
    val reward = returnedData(1).asInstanceOf[Double]

    done = returnedData(2).asInstanceOf[Boolean]

    reward
  }

  override def observeState:State = currentState

  override def observeStates = Seq.fill(possibleActions.size)(currentState) // TODO: Fix this, it should be transparent to the user

  override def finishedEpisode = done
}

object FrozenLakeV0 extends App {

  val numEpisodes = 5000
  val burnInEpisodes = 0
  val learningRate = 1
  val decay = 1

  val environment = new FrozenLakeV0

  def episodeGenerator():Option[Environment] = {
    environment.reset()
    Some(environment)
  }

  val policyIteration = new SARSA(episodeGenerator, numEpisodes, 1000, learningRate, decay)
  val activeActions:Set[Action] = Set(Up(), Down(), Left(), Right())
  val qFunction = new LinearApproximationValues(activeActions, new Discrete(0, 64).toFeatures.keySet)
  //val qFunction = new TabularValues()

  // Decaying epsilon
  val epsilon = 0.2
//  val lowerBound = 0.1
//  val eps = Decays.exponentialDecay(epsilon, lowerBound, numEpisodes, 0).iterator
  ///////////////////

  // Initial policy
  val initialPolicy = new EpGreedyPolicy(epsilon, qFunction)

  val iterationObserver = new EpisodeObserver {/**
    * Raised when an iteration finishes
    *
    * @param data about the current iteration
    */
  override def observeIteration(data: IterationObservation): Unit = {
    println(s"Iteration ${data.iterationNumber}")
    val environment = data.environment.asInstanceOf[FrozenLakeV0]
    environment.render()
  }

    /**
      * Raised when the episode finishes
      *
      * @param data about the episode at its end
      */
    override def episodeFinished(data: EpisodeObservation): Unit = Unit
}

  // Iterate the policy and it's convergence status
  val (learntPolicy, convergenceStatus) = policyIteration.iteratePolicy(initialPolicy)//, Some(iterationObserver))

  learntPolicy.save("frozenlakev0.json")
}