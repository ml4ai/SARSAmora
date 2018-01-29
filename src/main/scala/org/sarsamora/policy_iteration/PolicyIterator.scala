package org.sarsamora.policy_iteration

import com.typesafe.scalalogging.LazyLogging
import org.sarsamora.environment.Environment
import org.sarsamora.policies.{EpGreedyPolicy, Policy}

/**
  * environmentFabric:() => Option[Environment], episodeBound:Int, burnInEpisodes:Int
  * @param environmentFabric Fabric to fetch environments for running a new episode
  * @param episodeBound Maximun number of episodes to run policy iteration
  * @param burnInEpisodes Minumum number of episodes to run despite convergence status (Useful for delayed reward environments)
  */
abstract class PolicyIterator(environmentFabric:() => Option[Environment], episodeBound:Int, burnInEpisodes:Int) extends LazyLogging {

  // Stability flag controlling convergence
  var stable = true
  // Control variables
  var episodeCount = 0

  /**
    * Policy iteration algorithm
    * @param policy Policy to work with
    * @param episodeObserver Optional callback function receiving information each iteration
    * @return Learnt policy and convergence status
    */
  def iteratePolicy(policy:EpGreedyPolicy, episodeObserver:Option[EpisodeObserver] = None):(Policy, Boolean)

}
