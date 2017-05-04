package org.sarsamora.policies

import org.sarsamora.actions.Action

/**
  * Created by enrique on 31/03/17.
  */
class GreedyPolicy(override val values:Values) extends EpGreedyPolicy(0, values)
