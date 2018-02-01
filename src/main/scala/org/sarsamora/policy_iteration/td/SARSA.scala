package org.sarsamora.policy_iteration.td

import org.sarsamora.actions.Action
import org.sarsamora.environment._
import org.sarsamora.policies._
import org.sarsamora.states.State
import org.sarsamora.value_functions.ActionValues


/**
  * Iterates a policy by doing SARSA
  *
  * Created by enrique on 26/03/17.
  * @param environmentFabric Fabric with multiple environments, each call to it will return one environment
  *                          which encapsulates an episode. There's a 1-to-1 correspondence between
  *                          environments and episode numbers
  * @param episodeBound Upper limit on the number of episodes to iterate through
  * @param burnInEpisodes Minimum amount of episodes to iterate before stopping
  * @param alphas Learning rate stream
  * @param gamma Discount factor
  */
class SARSA(environmentFabric:() => Option[Environment], episodeBound:Int,
            burnInEpisodes:Int, alphas:Iterator[Double],
            gamma:Double = 0.8, lambda:Double = 1.0)
            extends OnlineTD(environmentFabric, episodeBound,
                      burnInEpisodes, alphas, gamma, lambda) {

  def this(environmentFabric:() => Option[Environment], episodeBound:Int, burnInEpisodes:Int, alpha:Double, gamma:Double, lambda:Double){
    this(environmentFabric, episodeBound, burnInEpisodes, Stream.continually[Double](alpha).iterator, gamma, lambda)
  }

  /**
    * Selects the next action given the current policy
    *
    * @param nextState       State to condition the actions
    * @param possibleActions Set of actions to chose from
    * @param policy          Policy being iterated
    * @return Chosen action
    */
  override protected def selectNextAction(nextState: State,
                                          possibleActions: Iterable[Action],
                                          policy: Policy,
                                          actionValues: ActionValues): Action = policy.selectAction(nextState, possibleActions.toSeq)
}