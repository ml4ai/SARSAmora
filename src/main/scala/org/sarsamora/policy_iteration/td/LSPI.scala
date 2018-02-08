package org.sarsamora.policy_iteration.td

import breeze.linalg.{DenseMatrix, DenseVector}
import org.sarsamora.actions.Action
import org.sarsamora.environment.Environment
import org.sarsamora.policies.{EpGreedyPolicy, Policy}
import org.sarsamora.policy_iteration.td.value_functions.TDUpdate
import org.sarsamora.policy_iteration.{EpisodeObservation, EpisodeObserver, IterationObservation}
import org.sarsamora.states.State
import org.sarsamora.policy_iteration.td.value_functions.LinearApproximationActionValues
import org.sarsamora.value_functions.ActionValues

import scala.collection.mutable

/**
  * Least-Squares Policy Iteration.
  *
  * Offline learning algorithm with linear function approximation.
  * The original paper for LSPI can be found at https://papers.nips.cc/paper/2134-model-free-least-squares-policy-iteration.pdf
  *
  * @param environmentFabric Fabric with multiple environments, each call to it will return one environment
  *                          which encapsulates an episode. There's a 1-to-1 correspondence between
  *                          environments and episode numbers
  * @param batchSize Number of experience samples to collect before doing policy improvement.
  *                  If set to 1, it's equivalent to an incremental algorithm.
  * @param featureNumber Number of basis functions (features) used on the linear approximation function
  * @param actionNumber Cardinality of the action space
  * @param gamma Decay parameter of TD learning
  * @param episodeBound Upper limit on the number of episodes to iterate through
  * @param burnInEpisodes Minimum amount of episodes to iterate before stopping
  */
class LSPI(environmentFabric:() => Option[Environment],
           batchSize:Int, featureNumber:Int, actionNumber:Int, gamma:Double,
           episodeBound:Int, burnInEpisodes:Int) {

  // Get the first episode and count how many we've executed
  private var episodeCount = 1

  var currentEpisode:Option[Environment] = environmentFabric()

  var stable = true

  // TODO: Document this method
  def sampleStream(policy: Policy, episodeObserver: Option[EpisodeObserver]):Iterable[(State, Action, Double, State, Set[Action])] =  {
    val batch = new mutable.ListBuffer[(State, Action, Double, State, Set[Action])]
    var samples = 0
    var iterationCounter = 0
    while(samples < batchSize && episodeCount <= episodeBound &&  currentEpisode.isDefined){
      currentEpisode match {
        case Some(env) =>
          if(env.finishedEpisode) {
            val prevEpisode = currentEpisode
            currentEpisode = environmentFabric()
            episodeCount += 1

            episodeObserver match {
              case Some(observer) =>
                // Fill the observation
                val observation = EpisodeObservation(prevEpisode.get, iterationCounter, episodeCount, currentEpisode.isEmpty, stable)
                // Pass to observer
                observer.episodeFinished(observation)
              case None => Unit
            }
            iterationCounter = 0
          }
          else{
            // Get a sample
            val currentState = env.observeState
            val currentAction = policy.selectAction(currentState, env.possibleActions)
            val reward = env.execute(currentAction)
            val nextState = env.observeState
            //val nextAction = policy.selectAction(nextState, env.possibleActions)

            batch.append((currentState, currentAction, reward, nextState, env.possibleActions.toSet))
            samples += 1
            iterationCounter += 1

            // Call the observer for this iteration
            episodeObserver match {
              case Some(observer) =>
                // Fill the observation
                val observation = IterationObservation(env, iterationCounter,
                  0.0, gamma,
                  currentState, currentAction,
                  reward, nextState, currentAction // TODO: This action is spurious, consider that ih the API
                )

                // Pass to observer
                observer.observeIteration(observation)
              case None => Unit
            }
          }
        case None => Unit
      }
    }
    batch
  }

  /**
    * Policy iteration algorithm
    * @param policy Policy to work with
    * @param episodeObserver Optional callback function receiving information each iteration
    * @return Learnt policy and convergence status
    */
  def iteratePolicy(policy:EpGreedyPolicy, trainingData:Option[Iterable[(State, Action, Double, State, Set[Action])]] = None, episodeObserver:Option[EpisodeObserver] = None):(Policy, Boolean)= {


    val values = policy.values match {
      case linearValues:LinearApproximationActionValues => linearValues
      case _ => throw new Exception("LSPI only support linear approximation values") // TODO: Specialize exception types
    }

    // Observe the training samples
    val batch = trainingData match {
      case Some(data) => data
      case None => sampleStream(policy, episodeObserver)
    }

    var iterations = 0

    do {

      stable = true

      val phi = new DenseMatrix[Double](batch.size,
        featureNumber*actionNumber,
        batch.flatMap(
          sample => values.valuesToArray(sample._1.toFeatures, Some(sample._2)).toArray).toArray)

      val rewards = new DenseVector[Double](batch.map(sample => sample._3).toArray)

      // Build the new sample to do LSQ

      val data = batch.flatMap{
        sample =>
          val nextAction = selectNextAction(sample._4, sample._5, policy, values)
          values.valuesToArray(sample._4.toFeatures, Some(nextAction)).toArray}.toArray

      val pPhi = new DenseMatrix[Double](batch.size,
        featureNumber*actionNumber,
        data
      )




      // Do the LSQ update
      val changed = values.lsUpdate(phi, pPhi, rewards, gamma)
      //val changed = values.lsUpdate2(batch, gamma, featureNumber*actionNumber)

      if(changed){
        stable = false
      }

      // Sample a new batch
      //batch ++= sampleStream(policy, episodeObserver)
      iterations +=1
      println(s"Iteration: $iterations")

    }while(!stable && iterations <= 100)

    // Return the tuned policy and the convergence status
    (policy, stable)
  }

  /**
    * Selects the next action as the greedy choice of the policy
    *
    * @param nextState       State to condition the actions
    * @param possibleActions Set of actions to chose from
    * @param policy          Policy being iterated
    * @return Chosen action
    */
  protected def selectNextAction(nextState: State,
                                          possibleActions: Iterable[Action],
                                          policy: Policy,
                                          actionValues:ActionValues): Action = {
    // Evaluate the possible choices
    val pairs = possibleActions map { a=> (nextState, a)}

    // Sort the pairs by their score
    val sortedPairs = pairs.toSeq.sortBy(actionValues.apply).reverse

    // Return the top choice
    sortedPairs.head._2
  }
}
