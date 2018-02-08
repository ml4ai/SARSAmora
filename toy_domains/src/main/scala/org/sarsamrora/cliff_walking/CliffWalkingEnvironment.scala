package org.sarsamrora.cliff_walking

import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.states.State

class CliffWalkingEnvironment(x:Int, y:Int) extends Environment{

  import CliffWalkingEnvironment._
  def this(){
    this(0,3)
  }
  def this(cell:Int) = {
    this(cell % 12, (cell/12).floor.toInt)
  }

  var iterationCounter = 0


  // Initialize the environment
  var currentState:CliffWalkingState = CliffWalkingState(x, y)
  var done = false



  /**
    * Renders gym's environment
    */
  def render():Unit = {
    0 to 3 foreach {
      j =>
        val x = 0 to 11 map {
          i =>
            val s = CliffWalkingState(i, j)

            if(inCliff(s))
              "C"
            else if(s == currentState)
              "x"
            else if(s == goal)
              "T"
            else
              "o"
        }
        val line = x.mkString("  ")
        println(line)
    }
    println()
  }

  /**
    * Possible actions to take at the current state of the environment
    * @return Sequence with CliffWalking actions
    */
  override def possibleActions = Seq(Down, Up, Left, Right)


  private def inCliff(state:CliffWalkingState):Boolean = state match {
    case CliffWalkingState(a, b) => b == 3 && a > 0 && a < 11
  }

  /**
    * Controls the environment
    * @param action Action to take
    * @param persist Whether the outcome will persist on the state of this instance
    * @return Observed reward
    */
  override def execute(action: Action, persist: Boolean): Double = {

    iterationCounter += 1
    // Cast the action into a CliffWalkingAction
    val cliffWalkingAction = action.asInstanceOf[CliffWalkingAction]

    // Execute the logic
    val outcome = cliffWalkingAction match {
      case Up =>
      {
        val up = if(currentState.y == 0) 0 else currentState.y -1
        CliffWalkingState(currentState.x, up)
      }

      case Down =>
      {
        val down = if(currentState.y == 3) 3 else currentState.y +1
        CliffWalkingState(currentState.x, down)
      }
      case Left =>
      {
        val left = if(currentState.x == 0) 0 else currentState.x -1
        CliffWalkingState(left, currentState.y)
      }
      case Right =>
      {
        val right = if(currentState.x == 11) 11 else currentState.x +1
        CliffWalkingState(right, currentState.y)
      }
    }

    if(this.inCliff(outcome)){
      currentState = origin
      -100d
    }
    else if(currentState == goal){
      0d // Don't change state if this is the goal (it's absorbing)
    }
    else{
      currentState = outcome
      if(currentState == goal)
        done = true
      -1
    }

  }

  /**
    * Current state of the environment
    * @return Instance of DiscreteState
    */
  override def observeState:State = currentState


  /**
    * Whether the episode has finished
    * @return
    */
  override def finishedEpisode:Boolean = done //|| iterationCounter > 10000

  /**
    * Describes the environment configuration on a string
    * @return
    */
  override def toString: String = "cliff_walking"
}

object CliffWalkingEnvironment{
  val origin = CliffWalkingState(0, 3)
  val goal = CliffWalkingState(11, 3)
}