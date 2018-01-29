package com.openai.gym

import jep.Jep
import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.Module
import org.sarsamora.environment.Environment


/***
  * Base class for all gym environments interfacing though Jep and ScalaPy
  */
abstract class JepEnvironment extends Environment{

  /**
    * Instance of the python interpreter. Implicit as ScalaPy requires one
    */
  implicit lazy val interpreter:Jep = new Jep()

  /**
    * Handle to python's gym module
    */
  val gym:Module = py.module("gym")

  /**
    * Resets gym's environment
    */
  def reset():Unit


  /**
    * Renders gym's environment
    */
  def render():Unit

}
