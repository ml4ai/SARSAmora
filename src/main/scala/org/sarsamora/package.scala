package org

import breeze.stats.distributions.RandBasis

import scala.util.Random

/**
  * Created by enrique on 18/04/17.
  */
package object sarsamora {
  val randGen = RandBasis.mt0
  val scalaRand = new Random(0)
}
