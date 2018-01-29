package org.sarsamora.policy_iteration.mc.value_functions

import org.sarsamora.actions.Action
import org.sarsamora.convergenceTolerance
import org.sarsamora.states.State

/**
  * Interface to be implemented by action-value functions in a Monte Carlo learning setting
  */
trait MCUpdate {
  /**
    * Monte Carlo update rule
    *
    * @param sample: Sequence of triplets with the observed states, actions and rewards
    * @param firstVisit Considers only the return for the first visit of a given state
    * @param tolerance tolerance parameter for convergence
    * @return Convergence status
    */
  def mcUpdate(sample:Seq[(State, Action, Double)], firstVisit:Boolean, tolerance:Double = convergenceTolerance):Boolean
}
