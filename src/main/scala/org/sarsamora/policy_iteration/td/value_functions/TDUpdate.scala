package org.sarsamora.policy_iteration.td.value_functions

import org.sarsamora.actions.Action
import org.sarsamora.states.State
import org.sarsamora.value_functions.ActionValues

/**
  * Trait to be implemented for any action-value function implementation used by temporal-difference learning
  */
trait TDUpdate extends ActionValues {
  /**
    * Perform the Bellman update for the particular implementation of the Q-function
    *
    * @param current Current state and value
    * @param next Next state and value
    * @param reward Observed reward
    * @param rate Learning rate (alpha)
    * @param decay Expected return decay (gamma)
    * @param lambda Lambda parameter for the elegibility traces (i.e. SARSA(Lambda))
    * @return Convergence status
    */
  def tdUpdate(current:(State, Action), next:(State, Action), reward:Double, rate:Double, decay:Double, lambda:Double):Boolean
}
