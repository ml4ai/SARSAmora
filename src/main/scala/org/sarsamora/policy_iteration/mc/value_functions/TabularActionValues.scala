package org.sarsamora.policy_iteration.mc.value_functions

import org.sarsamora.actions.Action
import org.sarsamora.states.State
import org.sarsamora.value_functions

import scala.collection.mutable

/**
  * TabularActionValues specialization for MC learning
  */
class TabularActionValues extends value_functions.TabularActionValues with MCUpdate {

  val returnMemory = new mutable.HashMap[(State, Action), mutable.ListBuffer[Double]]

  /**
    * Monte Carlo update rule for the tabular action-value function
    *
    * @param sample: Sequence of triplets with the observed states, actions and rewards
    * @param firstVisit Considers only the return for the first visit of a given state
    * @param tolerance  tolerance parameter for convergence
    * @return Convergence status
    */
  override def mcUpdate(sample:Seq[(State, Action, Double)], firstVisit: Boolean, tolerance: Double):Boolean = {

    var changed = false

    val (states, actions, rewards) = (sample.map(_._1), sample.map(_._2), sample.map(_._3))

    // Compute the cumulative rewards
    val returns = rewards.scanRight(0d)((a, b) => a+b).dropRight(1)

    val visited = new mutable.HashSet[(State, Action)]

    for(ix <- states.indices){
      val (state, action) = (states(ix), actions(ix))

      if(!firstVisit || (firstVisit && !visited.contains((state, action)))){
        val memory = returnMemory.getOrElse((state, action), new mutable.ListBuffer[Double])
        memory += returns(ix)
        val prevVal = backEnd((state, action))
        val newVal = memory.sum / memory.size
        if(Math.abs(newVal - prevVal) > tolerance){
          backEnd((state, action)) = newVal
          changed = true
        }
      }
    }


    changed
  }
}
