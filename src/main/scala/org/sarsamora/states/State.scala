package org.sarsamora.states

/**
  * Created by enrique on 26/03/17.
  */


trait State {
  def toFeatures:Map[String, Double]
}

trait StateParser {
  def fromString(description:String):State
}

