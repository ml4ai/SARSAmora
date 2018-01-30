package org.sarsamora.policies

import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.native.JsonMethods._
import org.sarsamora.actions.Action
import org.sarsamora.states.State
import org.sarsamora.value_functions.{ActionValueLoader, ActionValues}

import scala.language.implicitConversions

/**
  * Created by enrique on 26/03/17.
  *
  * Represents a policy to select an action given a state
  */
abstract class Policy {

//  /**
//    * Given a sequence of potentially different states and a sequence of actions, pick a pair to direct the environment
//    * @param ss Sequence of actions. Usually they will be the same, but this method allows to use a different state
//    *           paired with particular actions. Should have the same length as the sequence of possible actions
//    * @param possibleActions Sequence of actions that can be taken at the current moment
//    * @return Pair of state an action that were chosen by the policy.
//    */
//  def selectAction(ss:Seq[State], possibleActions:Seq[Action]):(State, Action)

  /**
    * Selects an action given the current state of the environment
    * @param s Current state
    * @param possibleActions Sequence of possible actions to chose from
    * @return Pair with the current state and the action chosen by the policy
    */
  def selectAction(s:State, possibleActions:Seq[Action]):Action

  // Save the policy as json
  def save(path:String)
}

/**
  * Companion class to load a policy from a json file
  */
object Policy {

  // This is needed by json4s
  implicit lazy val formats: DefaultFormats.type = DefaultFormats

  /**
    * Builds a policy out of a JObject instance
    * @param ast Abstract syntax tree containing the definition of the policy
    * @param actionValueLoader Implements the specific logic to build the correct type of ActionValues
    * @return Deserialized policy
    */
  def loadPolicy(ast:JObject, actionValueLoader: ActionValueLoader):Policy = {
    // Figure out the type of policy and put together an instance
    (ast \ "type").extract[String] match {
      // Epsilon-greedy policies
      case "ep_greedy" =>
        // Fetch the exploration parameter as a double
        val epsilon = (ast \ "epsilon").extract[Double]
        // Build the action-value function aided by the specified action-value loader
        val values = ActionValues.loadValues((ast \ "values").extract[JObject], actionValueLoader)
        // Return the instance of the policy
        new EpGreedyPolicy(epsilon, values)
      case _ =>
        throw new NotImplementedError("Not yet implemented")
    }
  }

  /**
    * Builds a policy out of a json file
    * @param path Path to the json file describing the policy
    * @param actionValueLoader Implements the specific logic to build the correct type of ActionValues
    * @return Deserialized policy
    */
  def loadPolicy(path:String, actionValueLoader: ActionValueLoader):Policy = {
    // Read the contents of the json file
    val text = io.Source.fromFile(path).getLines.mkString("\n")
    // Parse the text into a json ast
    val json = parse(text).asInstanceOf[JObject]
    // Build the policy out of the resulting ast
    loadPolicy(json, actionValueLoader)
  }
}