#' Analyze one condition
#'
#' @param df A dataframe that must have fields targetSP and SPE
#' @param numItemsInStream Number of items in the stream (assumed to be the same for all trials)
#' @param paramBounds With fields lower and upper, each of length two indicating min and max val for efficacy,latency,precision
#' @param nReplicates How many times to fit the model (with different random starting points within paramBounds
#' @return efficay, latency, precision, val (negative log likelihood), and warnings (currently disabled, don't know why)
#' @examples
#' analyzeOneCondition(subset(P2E2pilot,subject=="CB" & target==1 & condition==1),16,parameterBounds(),1)
#'
#' @export
analyzeOneCondition<- function(df, numItemsInStream, paramBounds, nReplicates=3) {
  #nReplicates. Number of times to repeat each fit with different starting values. Pat maybe used 100
  # Calculate the domain of possible serial position errors.
  possibleTargetSP<- sort(unique(df$targetSP))
  minTargetSP <- min(possibleTargetSP)
  maxTargetSP <- max(possibleTargetSP)
  minSPE <- 1 - maxTargetSP
  maxSPE <- numItemsInStream - minTargetSP

  #calculate the guessing distribution, empirically (based on actual targetSP)
  pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)

  # Set some model-fitting parameters.
  fitMaxIter <- 10^4# Maximum number of fit iterations
  fitMaxFunEvals <- 10^4# Maximum number of model evaluations

  #Use RT to check which is left target and which is right target
  fitModelDF <- function( SPE, minSPE, maxSPE ) {
    #Calculate parameter guess
    startingParams<- parametersGuess( paramBounds$lower, paramBounds$upper )
    fit<- fitModel(SPE, minSPE, maxSPE, pseudoUniform, startingParams, paramBounds)
    fit<- fit$content
    warns<- fit$warnings
    #print(fit)
    return( data.frame(efficay=fit[1], latency=fit[2], precision=fit[3], val=fit$value, warnings=warns) )
  }

  for (n in 1:nReplicates) { #fit the model many times (with different starting parameters)

    paramsPlusNegLogLikelihood<- fitModelDF( df$SPE, minSPE, maxSPE )
    #print(paramsPlusNegLogLikelihood)
    #Save the best estimate
    if (n==1) {
      bestEstimate <- paramsPlusNegLogLikelihood
    } else {
      if (paramsPlusNegLogLikelihood$val < bestEstimate$val)
      { bestEstimate <- paramsPlusNegLogLikelihood }
    }
  }  #End fitting the model many times with different starting params

  return( bestEstimate )
}



