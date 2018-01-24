package org.sarsamora.gym

import org.sarsamora.environment.Environment
import jep.Jep
import me.shadaj.scalapy.py


abstract class JepEnvironment extends Environment{

  implicit val interpeter = new Jep()
  val gym = py.module("gym")


  //override def possibleActions(): Seq[Action] = ???

//  override def executePolicy(action: Action, persist: Boolean): Double = ???
//
//  override def observeState: State = ???
//
//  override def observeStates: Seq[State] = ???
//
//  override def finishedEpisode: Boolean = ???

}
