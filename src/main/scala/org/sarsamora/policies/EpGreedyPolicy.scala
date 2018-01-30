package org.sarsamora.policies

import java.io.{BufferedWriter, FileWriter}

import breeze.linalg._
import breeze.stats.distributions.{Multinomial, RandBasis}
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.sarsamora.actions.Action
import org.sarsamora.randGen
import org.sarsamora.states.State
import org.sarsamora.value_functions.ActionValues

/**
  * Epsilon-Greedy policy does a greedy choice with 1-epsilon probability and a non-greedy choice
  * @param epsilons Iterator with the exploration probability. Being this way allows for a decay of expsilon
  * @param values Action-Value function implementation used for computing the greedy choice
  */
class EpGreedyPolicy(epsilons:Iterator[Double], val values:ActionValues) extends Policy {


  /**
    * Auxiliary constructor that allows for a fixed epsilon parameter
    * @param epsilon Exploration probability
    * @param values Action-Value function implementation used for computing the greedy choice
    */
  def this(epsilon:Double, values:ActionValues){
    // Call the main constructor by creating an infinit stream with repeated values of epsilon
    this(Stream.continually[Double](epsilon).iterator, values)
  }

  // The first value of epsilon
  var firstEpsilon:Option[Double] = None


  /**
    * Select an action given the current state of the environment
    *
    * @param s Sequence of actions. Usually they will be the same, but this method allows to use a different state
    *           paired with particular actions. Should have the same length as the sequence of possible actions
    * @param possibleActions Sequence of actions that can be taken at the current moment
    * @return Pair of state an action that were chosen by the policy.
    */
  override def selectAction(s:State, possibleActions:Seq[Action]):Action = {


    // Fetch the next value of epsilon
    val epsilon = epsilons.next()

    // Is this is the first time, memorize the first value of epsilon
    if(firstEpsilon.isEmpty)
      firstEpsilon = Some(epsilon)

    // Make sure the value of epsilon is a probability
    assert(epsilon <= 1 && epsilon >= 0, s"Invalid Epsilon value: $epsilon")

    // Is there a more idiomatic way to do this in scala?
    val numActions = possibleActions.size

    // Compute the greedy probability
    val greedyProb = 1 - epsilon
    // Probability of a given non-greedy choice
    val nonGreedySlice = if(numActions > 1) epsilon/(numActions-1) else 0.0

    // Pair together the states and the actions
    val stateActions:Seq[(State, Action)] = possibleActions map (a => (s, a))
    // Evaluate them according to the action-value function
    val stateActionValues = stateActions map (k => values(k))
    // Sort the actions decreasingly according to their q-value
    val sortedActions = stateActions.zip(stateActionValues).sortBy{case(sa, v) => v}.map(_._1._2).reverse
    // Compute the multinomial parameter to make the action choice
    val probs = (greedyProb::List.fill(numActions-1)(nonGreedySlice)).toArray

    // Do a random sample from a multinomial distribution using probs as parameter
    implicit val rand: RandBasis = randGen // This sets the random number generator for the
    val dist = new Multinomial(DenseVector(probs))

    // Sample an index from the multinomial distributions
    val choiceIx = dist.sample
    // Fetch the action corresponding to the sampled index
    val choice = sortedActions(choiceIx)

    // Return the random sample
    choice
  }

  /**
    * Persists the current policy into a json file
    * @param path to the saved policy
    */
  override def save(path:String): Unit ={
    // Generate a Json ast
    val ast = {
      ("type" -> "ep_greedy") ~
      ("epsilon" -> firstEpsilon.getOrElse(0.0)) ~
        ("values" -> values.toJson)
    }

    // Create the text representation of the ast
    val json = pretty(render(ast))

    // Save it into disk
    val bfw = new BufferedWriter(new FileWriter(path))
    bfw.write(json)
    bfw.close()
  }

  /**
    * Creates a greedy version of this policy, meaning epsilon = 0
    * @return Greedy policy with the same action-value function
    */
  def makeGreedy:GreedyPolicy = new GreedyPolicy(values)
}
