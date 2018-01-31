package org.sarsamora.value_functions

import org.json4s.JsonAST.JObject
import org.json4s._
import org.sarsamora.actions._
import org.sarsamora.states.{State, StateParser}

import scala.language.implicitConversions

/**
  * Created by enrique on 26/03/17.
  */


/**
  * Base class for any ActionValue function
  */
abstract class ActionValues{

  /**
    * Returns the action-value (q-value) of a state-action pair
    * @param key State-Action pair to be evaluated
    * @return The value of the input pair
    */
  def apply(key:(State, Action)):Double

  /**
    * Converts the current Action-Value function into a Json AST for serialization
    * @return
    */
  def toJson:JObject
}

/**
  * Trait to be implemented by an object that deserializes a particular implementation of ActionValues
  */
trait ActionValueLoader{
  /**
    * Instance of the state parser for the particular domain of the ActionValues instance
    */
  val stateParser:StateParser

  /**
    * Builds a map of Actions to a map of feature-values to be fed into the instance of ActionValues
    * @param ast The Json representation of the values
    * @return Parsed features
    */
  def loadActionValues(ast:JObject):Map[Action, collection.Map[String, Double]]
}


object ActionValues{

  /**
    * Calls the appropriate loaders to return the instance of the ActionValues function
    * @param ast Json representation of the action values
    * @param actionValueLoader Instance that implements the logic to parse the serialized values
    * @return Built concrete instance of the action values
    */
  def loadValues(ast:JObject, actionValueLoader: ActionValueLoader):ActionValues = {
    ast \ "type" match {
      case JString("linear") =>

        val coefficientsMap = actionValueLoader.loadActionValues(ast)
        new LinearApproximationActionValues(coefficientsMap)

      case JString("tabular") =>
        val functionTable = actionValueLoader.loadActionValues(ast)
        new TabularActionValues(functionTable, actionValueLoader.stateParser)

      case _ =>
        throw new NotImplementedError("Not yet implemented")
    }
  }
}







