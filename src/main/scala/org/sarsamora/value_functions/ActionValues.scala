package org.sarsamora.value_functions

import org.json4s.JsonAST.JObject
import org.json4s._
import org.sarsamora.actions._
import org.sarsamora.states.{State, StateParser}

import scala.language.implicitConversions

/**
  * Created by enrique on 26/03/17.
  */


abstract class ActionValues{

  def apply(key:(State, Action)):Double
  def toJson:JObject
}

trait ActionValueLoader{
  val stateParser:StateParser
  def loadActionValues(ast:JObject):Map[Action, collection.Map[String, Double]]
}

object ActionValues{

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







