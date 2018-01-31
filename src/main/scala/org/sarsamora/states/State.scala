package org.sarsamora.states

/**
  *
  */


/**
  * Base trait extended by concrete state objects
  *
  * Created by enrique on 26/03/17.
  */
trait State {
  /**
    * Convert a particular state into it's features representation for learning of a function approximation
    * @return Map of feature names to feature values
    */
  def toFeatures:Map[String, Double]
}

/**
  * Trait to be implemented by a class that parses a string into a particular state instance
  */
trait StateParser {
  def fromString(description:String):State
}

