package org.sarsamora.chain_walk

import org.sarsamora.states.State

case class ChainWalkingState(id:Int) extends State{
  /**
    * Convert a particular state into it's features representation for learning of a function approximation
    *
    * @return Map of feature names to feature values
    */
  override def toFeatures = Map("s" -> id, "s^2" -> id*id)
}
