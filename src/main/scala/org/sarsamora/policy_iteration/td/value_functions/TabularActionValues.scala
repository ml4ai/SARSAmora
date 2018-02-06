package org.sarsamora.policy_iteration.td.value_functions

import org.sarsamora.actions.Action
import org.sarsamora.states.State
import org.sarsamora.{convergenceTolerance, value_functions}


/**
  * Specialization of the tabular representation of the Q-function with the TD update
  * @param tolerance Convergence tolerance parameter
  */
class TabularActionValues(val tolerance:Double = convergenceTolerance) extends value_functions.TabularActionValues
  with TDUpdate {

  // TODO: Implement eligibility traces

  /**
    * Bellman update for the tabular q-function
    * @param current current state and value
    * @param next next state and value
    * @param reward observed reward
    * @param rate learning rate (alpha)
    * @param decay TD decay (gamma)
    * @param lambda elegibility traces scaling factor (lambda)
    * @return Convergence status
    */
  override def tdUpdate(current:(State, Action), next:(State, Action),
                        reward:Double, rate:Double,
                        decay:Double, lambda:Double):Boolean = {

    // Fetch the current value
    val value:Double = this(current)

    // Bellman update
    val newValue:Double = value + (rate*(reward + decay*this(next) - value))

    // Store the update in the memory back-end
    backEnd(current) =  newValue

    // Return whether the current changed above the requested tolerance
    Math.abs(newValue - value) > tolerance
  }
}
