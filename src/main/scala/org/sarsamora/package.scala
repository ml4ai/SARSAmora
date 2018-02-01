package org

import breeze.stats.distributions.RandBasis

import scala.util.Random

/**
  * Created by enrique on 18/04/17.
  */

/**
  * A place to put global variables
  */
package object sarsamora {
  val randGen: RandBasis = RandBasis.mt0
  val scalaRand = new Random(0)
  val convergenceTolerance = 1e-4
}
