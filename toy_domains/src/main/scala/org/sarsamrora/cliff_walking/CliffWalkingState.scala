package org.sarsamrora.cliff_walking

import breeze.linalg.DenseVector
import org.sarsamora.states.{State, StateParser}

import scala.collection.mutable

case class CliffWalkingState(x:Int, y:Int) extends State{

  import CliffWalkingEnvironment.{goal,origin}

  val coordinate = (x, y)


  private def distance(x:(Int, Int)) = CliffWalkingState.distance(coordinate, x)

  private def neighbors = {
    var left = x-1
    var right = x+1
    var up = y+1
    var down = y-1

    left = if(left < 0) 0 else left
    right = if(right > 11) 11 else right
    up = if(up < 0) 0 else up
    down = if(down > 3) 3 else down

    //Left, right, up, down
    Seq((left, y), (right, y), (x, up), (x, down))
  }


  /**
    * Convert a particular state into it's features representation for learning of a function approximation
    *
    * @return Map of feature names to feature values
    */
  override def toFeatures: Map[String, Double] = {
    import CliffWalkingState.stateFeatures
    if(stateFeatures.contains(this)){
      stateFeatures(this)
    }
    else{
      val features = this.toLargeFeatures
      stateFeatures += this -> features
      features
    }
  }

  lazy val toSmallFeatures:Map[String, Double] = {
    // All distances are manhattan

    val fromOrigin = CliffWalkingState.difference(coordinate, origin.coordinate)
    val fromGoal = CliffWalkingState.difference(coordinate, goal.coordinate)
    val cliffBelow = if(y == 2 && (x >0 && x< 11)) 1d else 0d
    val cliffBesides = if(coordinate == origin.coordinate) 1d else 0d
    val mGoal = this.distance(goal.coordinate)

    val n = neighbors
    val dRight = distance(n(1))
    val dLeft = distance(n(0))
    val dUp = distance(n(2))
    val dDown = distance(n(3))

    //val visited = neighbors map ( c => if(CliffWalkingState.visitedStates.contains(CliffWalkingState(c))) 1.0 else 0.0)

    Map(
      //"fromOriginX" -> fromOrigin._1 / 3,
      //"fromOriginY" -> fromOrigin._2 / 11,
      "fromGoalX" -> fromGoal._1 / 3,
      "fromGoalY" -> fromGoal._2 / 11,
      "cliffBelow" -> cliffBelow,
      "cliffBesides" -> cliffBesides
      //      "dLeft" -> dLeft / 14,
      //      "dRight" -> dRight / 14,
      //      "dUp" -> dUp / 14,
      //      "dDownCB" -> dDown / 14 * cliffBelow,
      //      "dRightCB" -> dRight / 14 * cliffBesides
      //      "vLeft" -> visited(0),
      //      "vRight" -> visited(1),
      //      "vUp" -> visited(2),
      //      "vDown" -> visited(3)
    )
  }

  lazy val toLargeFeatures:Map[String, Double] = {
    (0 to 47).map{
      i =>
        val cell = CliffWalkingState(i)
        cell.toString -> (if(cell.coordinate == coordinate) 1.0 else 0.0)
    }.toMap
  }

}

object CliffWalkingState extends StateParser{

  val stateFeatures = new mutable.HashMap[CliffWalkingState, Map[String, Double]]

  def apply(cell:Int):CliffWalkingState = {
    CliffWalkingState(cell % 12, (cell/12).floor.toInt)
  }

  def apply(coordinate:(Int, Int)):CliffWalkingState = {
    CliffWalkingState(coordinate._1, coordinate._2)
  }

  private def distance(a:(Int, Int), b:(Int, Int)):Int = Math.abs(b._1 - a._1) + Math.abs(b._2 - a._2)

  private def difference(x:(Int, Int), y:(Int, Int)) = (y._1 - x._1, y._2 - x._2)

  override def fromString(description: String) = {
    val discreteState = raw"CliffWalkingState\((\d+),(\d+)\)".r

    description match {
      case discreteState(currentState, numStates) =>
        CliffWalkingState(currentState.toInt, numStates.toInt)
      case _ =>
        throw new Exception(s"State $description can not be parsed")
    }
  }
}
