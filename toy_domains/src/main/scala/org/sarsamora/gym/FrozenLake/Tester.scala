package org.sarsamora.gym.FrozenLake

import collection.mutable
import org.sarsamora.policies.{EpGreedyPolicy, Policy}

object Tester extends App {

  val valueLoader = new FrozenLakeActionsActionValues
  val policy = Policy.loadPolicy("frozenlakev0.json", valueLoader).asInstanceOf[EpGreedyPolicy].makeGreedy

  val environment = new FrozenLakeV0

  val trails = 1000
  var i = 0
  val cumulativeRewards = new mutable.ArrayBuffer[Double]()

  while(i < trails) {
    println(s"Entering trail $i")
    var reward = 0.0

    environment.reset()
    environment.render()

    while (!environment.done) {
      val currentState = environment.observeState
      val action = policy.selectAction(currentState, environment.possibleActions())._2
      reward += environment.executePolicy(action)
      environment.render()
    }

    //println(s"Reward: $reward")

    cumulativeRewards += reward
    i+= 1
  }

  println(s"Average reward out of $trails trails: ${cumulativeRewards.sum/cumulativeRewards.size}")
}
