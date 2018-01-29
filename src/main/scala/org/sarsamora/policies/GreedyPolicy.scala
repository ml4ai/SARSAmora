package org.sarsamora.policies

import org.sarsamora.value_functions.ActionValues

/**
  * Created by enrique on 31/03/17.
  *
  * Specific policy that always takes the greedy choice. Implemented as a subclass of EpGreedyPolicy giving a zero
  * probability of selecting a suboptimal choice
  *
  * @param values Action-Value function used to selecg the greedy choice
  */
class GreedyPolicy(override val values:ActionValues) extends EpGreedyPolicy(0, values)
