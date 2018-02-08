package org.sarsamora.chain_walk.exec

import breeze.stats.distributions.{Bernoulli, RandBasis, Uniform}
import org.sarsamora.actions.Action
import org.sarsamora.chain_walk.{ChainWalkingEnvironment, ChainWalkingState, Left, Right}
import org.sarsamora.policies.EpGreedyPolicy
import org.sarsamora.policy_iteration.{EpisodeObservation, EpisodeObserver, IterationObservation}
import org.sarsamora.policy_iteration.td.LSPI
import org.sarsamora.policy_iteration.td.value_functions.LinearApproximationActionValues
import org.sarsamora.states.State

object Train extends App{

  implicit val rand: RandBasis = org.sarsamora.randGen

  // Collect samples
  val sampleSize = 105

  val dist = new Bernoulli(0.5)
  val environment = new ChainWalkingEnvironment


  println("Collecting samples ...")

  val batch = 0 to sampleSize map {
    _ =>
      val action = dist.sample match {
        case true => Right
        case false => Left
      }

      val state = environment.observeState
      val reward = environment.execute(action)
      val newState = environment.observeState

      (state, action, reward, newState, Set[Action](Right, Left))
  }

  // Print sample distribution
//  batch.groupBy {
//    case (s,a,_,ss, ___) => (s, a, ss)
//  }.mapValues(_.size).toSeq.sortWith{
//    case (a, b) =>
//      val (k1, k2) = (a._1, b._1)
//      val v1 = (k1._1.id, k1._2, k1._3.id)
//      val v2 = (k2._1.id, k2._2, k2._3.id)
//
//      v1 < v2
//  }.foreach{
//    case (k, v) =>
//      println(s"$k -> $v")
//  }

  val featuresNames = ChainWalkingState(1).toFeatures.keySet
  val qFunction = new LinearApproximationActionValues(Set(Left, Right), featuresNames, true)
  val policy = new EpGreedyPolicy(0.1, qFunction)

  val iterator = new LSPI(
    () => { Some(environment) },
    sampleSize,
    featuresNames.size + 1,
    2, 0.9, 10000, 0
  )

  val (learntPolicy, converged) = iterator.iteratePolicy(policy, Some(batch), Some(new EpisodeObserver {
      /**
      * Raised when an iteration finishes
      *
      * @param data about the current iteration
      */
      override def observeIteration(data: IterationObservation): Unit = {}

      /**
        * Raised when the episode finishes
        *
        * @param data about the episode at its end
        */
      override def episodeFinished(data: EpisodeObservation): Unit = {}
  }))

  if(converged) println("Converged!!")

  val greedyPolicy = learntPolicy.asInstanceOf[EpGreedyPolicy].makeGreedy
  val p = 0 until 4 map { i =>
     val s = new ChainWalkingState(i)
     val a = greedyPolicy.selectAction(s, environment.possibleActions)
     (s, a)
   }

  for((s, a) <- p){
    println(s"$s, $a")
  }
}
