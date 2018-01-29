package com.openai.gym.FrozenLake.exec

import com.openai.gym.FrozenLake.{FrozenLakeActionsActionValues, FrozenLakeEnvironment}
import org.sarsamora.policies.{EpGreedyPolicy, Policy}

import scala.collection.mutable

/**
  * Run a FrozenLake policy!!
  */
object Test extends App {

  // Get the name of the policy to run
  val policyPath = args(0)

  // Instantiate an action values' parser for frozen lake actions
  val valueLoader = new FrozenLakeActionsActionValues
  // Load a serialized policy and make it greedy
  val policy = Policy.loadPolicy(policyPath, valueLoader).asInstanceOf[EpGreedyPolicy].makeGreedy

  // Create a new FrozenLake environment that will run the policy
  val environment = new FrozenLakeEnvironment("4x4", true)

  // Number of trails to execute
  val trails = 100
  // Memory of the observed rewards
  val cumulativeRewards = new mutable.ArrayBuffer[Double]()

  // Execute many times the policy
  for(i <- 1 to trails) {
    println(s"Entering trail $i")
    // Reward of the episode
    var reward = 0.0

    // Initialize the environment by calling gym's reset
    environment.reset()
    // Draw the board
    environment.render()

    // Execute the policy until it finishes
    while (!environment.done) {
      // Observe the current state of the environment
      val currentState = environment.observeState
      // Select the action given the current state and the possible actions
      val action = policy.selectAction(currentState, environment.possibleActions)._2
      // Accumulate the observed reward
      reward += environment.execute(action)
      // Draw the board
      environment.render()
    }

    //println(s"Reward: $reward")
    // Keep track of the cumulative reward
    cumulativeRewards += reward
  }

  // Print the average cumulative reward of this policy
  println(s"Average reward out of $trails trails: ${cumulativeRewards.sum/cumulativeRewards.size}")
}
