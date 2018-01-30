package org.sarsamora.policy_iteration.td

// TODO: Factorize common code from Q-Learning and SARSA

///**
//  * Iterates a policy by doing Q-Learning
//  *
//  * Created by enrique on 26/03/17.
//  * @param environmentFabric Fabric with multiple environments, each call to it will return one environment
//  *                          which encapsulates an episode. There's a 1-to-1 correspondence between
//  *                          environments and episode numbers
//  * @param episodeBound Upper limit on the number of episodes to iterate through
//  * @param burnInEpisodes Minimum amount of episodes to iterate before stopping
//  * @param alpha Learning rate
//  * @param gamma Discount factor
//  */
//class QLearning(environmentFabric:() => Option[Environment], episodeBound:Int, burnInEpisodes:Int, alpha:Double = 0.01, gamma:Double = 0.8, lambda:Double = 1.0) extends LazyLogging {
//
//  // Stability flag controlling convergende
//  var stable = true
//  // Control variable
//  var episodeCount = 0
//  // Lower bound to the learning rate
//  val alphaFloor = 0.00001
//
//  // TODO: Parameterize this
//  // Cool-down schedule for the leraning rate
//  val alphas: Iterator[Double] = Decays.exponentialDecay(alpha, alphaFloor, episodeBound, 500).iterator
//
//
//  /**
//    * Policy iteration algorithm
//    * @param policy Policy to work with
//    * @param episodeObserver Optional callback function receiving information each iteration
//    * @return Learnt policy and convergence status
//    */
//  def iteratePolicy(policy:EpGreedyPolicy, episodeObserver:Option[EpisodeObserver] = None):(Policy, Boolean) = {
//
//    // Fetch the initial environment/episode
//    var episode = environmentFabric()
//
//    do {
//      stable = true
//
//      // Local control variable
//      var iterationCounter = 0
//
//      episode match {
//        // If there's an environment left to run this episode ...
//        case Some(environment) =>
//
//          val currentAlpha = alphas.next
//
//          // TODO: Explain why the initial state could be multiple states
//          // Observe the initial state
//          val possibleStates = environment.observeState
//
//          // Evaluate the policy
//
//
//          // Enter into the episode loop
//          while(!environment.finishedEpisode){
//
//            // TODO: Verify this step in the book!!
//            var currentState = possibleStates
//            var currentAction = policy.selectAction(possibleStates, environment.possibleActions)
//
//            // Execute chosen action and observe reward
//            val reward = environment.execute(currentAction)
//
//            // Observe the new state after executing the action
//            val possibleNextStates = environment.observeStates
//            val possibleNextActions = environment.possibleActions
//
//            // Evaluate all the q-values of the state/action pairs
//            val choices = (possibleNextStates zip possibleNextActions).sortBy{case (s, a) => policy.values(s, a)}.reverse
//
//            // Chose a new action greedy-ly
//            val (nextState, nextAction) = choices.head
//
//
//            // Perform the update
//            val actionValues = policy.values match {
//              case v:TDUpdate => v
//              case _ => throw new IllegalArgumentException("Action values don't implement TD update")
//            }
//
//            val changed = actionValues.tdUpdate((currentState, currentAction), (nextState, nextAction), reward, currentAlpha, gamma, lambda)
//
//            // Keep track of the fluctuations of the values
//            if(changed)
//              stable = false
//
//            iterationCounter += 1
//
//            // Call the observers for this episode
//            episodeObserver match {
//              case Some(observer) =>
//                // Fill the observation
//                val observation = IterationObservation(environment, iterationCounter,
//                  alpha, gamma,
//                  currentState, currentAction,
//                  reward, nextState, nextAction
//                )
//
//                // Pass to observer
//                observer.observeIteration(observation)
//              case None => Unit
//            }
//
//            // Update the state and action
//            currentState = nextState
//            currentAction = nextAction
//
//          }
//
//          // Increment the episode count
//          episodeCount += 1
//
//          if(episodeCount % 10 == 0)
//            logger.info(s"Episode $episodeCount")
//
//        // In case we ran out of environments, we're done
//        case None => Unit
//      }
//
//      val prevEpisode = episode
//      episode = environmentFabric()
//      // Call the observed if specified
//      episodeObserver match {
//        case Some(observer) =>
//          // Fill the observation
//          val observation = EpisodeObservation(prevEpisode.get, iterationCounter, episode.isEmpty, stable)
//          // Pass to observer
//          observer.episodeFinished(observation)
//        case None => Unit
//      }
//
//    }
//    // If the next environment is defined and if we're within the episode bounds and if it's unstable and if we went
//    // through the burn in process. Phew ....
//    while(episode.isDefined && (!stable || episodeCount <= burnInEpisodes) && episodeCount <= episodeBound)
//
//    if(stable)
//      logger.info(s"Converged on $episodeCount episodes")
//    else
//      logger.info(s"Didn't converge")
//
//    (policy, stable)
//  }
//}

