package org.sarsamora.chain_walk

import org.sarsamora.actions.Action

trait  ChainWalkingAction extends Action

object Left extends ChainWalkingAction {
  override def toString: String = "Left"
}

object Right extends ChainWalkingAction {
  override def toString: String = "Right"
}
