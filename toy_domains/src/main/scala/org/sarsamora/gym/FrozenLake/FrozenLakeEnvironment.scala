package org.sarsamora.gym.FrozenLake

import org.sarsamora.actions.Action
import org.sarsamora.states.State
import org.sarsamora.gym.JepEnvironment
import org.sarsamora.gym.observation_spaces.Discrete
import me.shadaj.scalapy.py.{DynamicObject, Module, Object}

import scala.collection.JavaConversions._

class FrozenLakeEnvironment(val map_name:String, val slippery:Boolean) extends JepEnvironment {

  // Import the frozen lake environment
  interpreter.eval("from gym.envs.toy_text.frozen_lake import FrozenLakeEnv")

  // Instantiate the environment
  private val envString = s"FrozenLakeEnv(is_slippery = ${if(slippery) "True" else "False"}, map_name='$map_name')"
  private val env = Object(envString).asInstanceOf[DynamicObject]

  // Get the number of states
  val cardinality = env.observation_space.n.value.asInstanceOf[Int]

  // Initialize the environment
  var currentState:Discrete = Discrete(0, cardinality)
  var done = false

  // Call the reset method on the gym environment (Python)
  env.reset()

  /**
    * Resets gym's environment
    */
  override def reset():Unit = {
    // Call the reset method on the gym environment (Python)
    env.reset()
    // Reset the state of the environment
    currentState = Discrete(0, cardinality)
    done = false
  }

  /**
    * Renders gym's environment
    */
  override def render():Unit = {
    env.render()
  }

  /**
    * Possible actions to take at the current state of the environment
    * @return Sequence with FrozenLake actions
    */
  override def possibleActions() = Seq(Up(), Down(), Left(), Right())

  /**
    * Controls the environment
    * @param action Action to take
    * @param persist Whether the outcome will persist on the state of this instance
    * @return Observed reward
    */
  override def executePolicy(action: Action, persist: Boolean) = {

    // Cast the action into a FrozenLakeAction
    val frozenAction = action.asInstanceOf[FrozenLakeAction]
    // Step gym's environment with the action
    val x = env.step(frozenAction.id)

    // Fetch the resulting data from python
    val returnedData = x.value.asInstanceOf[java.util.List[Any]]

    // Parse the resulting state
    val state = returnedData(0).asInstanceOf[Int]
    // Marshall it into our representation
    currentState = Discrete(state, cardinality)
    // Parse the observed reward
    val reward = returnedData(1).asInstanceOf[Double]

    // Take note if the episode is over
    done = returnedData(2).asInstanceOf[Boolean]

    // Return the observed rewards
    reward
  }

  /**
    * Current state of the environment
    * @return Instance of DiscreteState
    */
  override def observeState:State = currentState


  override def observeStates:Seq[State] = Seq.fill(possibleActions.size)(currentState) // TODO: Fix this, it should be transparent to the user

  /**
    * Whether the episode has finished
    * @return
    */
  override def finishedEpisode:Boolean = done

  /**
    * Describes the environment configuration on a string
    * @return
    */
  override def toString: String = envString
}