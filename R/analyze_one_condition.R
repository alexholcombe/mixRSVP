#Used by analyzeOneCondition and
doFit <- function( SPE, minSPE, maxSPE, pseudoUniform, paramBounds ) {
  #Calculate parameter guess
  startingParams<- parametersGuess( paramBounds$lower, paramBounds$upper )
  fit<- fitModel(SPE, minSPE, maxSPE, pseudoUniform, startingParams, paramBounds)
  warns<- fit$warnings
  fit<- fit$content
  fitMetric<- fit$value
  #Need to replace any NULL warnings with None, otherwise assigning this to other dataframes will delete that column
  fitNoNames<-as.numeric(fit) #Otherwise p1, p2, p3 names preserved, which is confusing
  return( list(efficacy=fitNoNames[1], latency=fitNoNames[2], precision=fitNoNames[3], val=fitMetric, warnings=warns) )
}

fitOneCondition<- function(df, numItemsInStream, paramBounds, nReplicates=3) {
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
  #fitMaxIter <- 10^4# Maximum number of fit iterations
  #fitMaxFunEvals <- 10^4# Maximum number of model evaluations

  for (n in 1:nReplicates) { #fit the model many times (with different starting parameters)

    paramsPlusNegLogLikelihood<- doFit( df$SPE, minSPE, maxSPE, pseudoUniform, paramBounds )
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

#' Analyze one condition
#'
#' @param df A dataframe that must have fields targetSP and SPE
#' @param numItemsInStream Number of items in the stream (assumed to be the same for all trials)
#' @param paramBounds With fields lower and upper, each of length two indicating min and max val for efficacy,latency,precision
#' @param nReplicates How many times to fit the model (with different random starting points within paramBounds)
#' @return efficay, latency, precision, val (negative log likelihood), and warnings (currently disabled, don't know why)
#' @examples
#' df <-  subset(P2E2pilot,subject=="CB" & target==1 & condition==1)
#' analyzeOneCondition(df, 16, parameterBounds(), 1)
#'
#' @export
analyzeOneCondition<- function(df, numItemsInStream, paramBounds, nReplicates=3) {
  #nReplicates. Number of times to repeat each fit with different starting values. Pat maybe used 100
  # Calculate the domain of possible serial position errors.

  bestEstimate<- fitOneCondition(df, numItemsInStream, paramBounds, nReplicates)
  answers<-bestEstimate

  #Check whether this fits significantly better than guessing distribution
  #and return both the statistical test and the likelihood difference
  negLogLikGuessing <- -1*logLikGuessing(df, numItemsInStream)
  bestEstimate$valGuessing <- negLogLikGuessing

  negLogLikMixture <-  bestEstimate$val
  #print(cat("negLogLikGuessing=",negLogLikGuessing," negLogLikMixture=",negLogLikMixture))
  pval <- likelihoodRatioTest(-1*negLogLikMixture,-1*negLogLikGuessing,3) #takes positive log likelihoods
  bestEstimate$pLRtest <- pval

  return( bestEstimate )
}

#' Analyze one condition, and return estimates as dataframe so suitable for use in dplyr pipelines
#'
#' @param df A dataframe that must have fields targetSP and SPE
#' @param numItemsInStream Number of items in the stream (assumed to be the same for all trials)
#' @param paramBounds With fields lower and upper, each of length two indicating min and max val for efficacy,latency,precision
#' @param nReplicates How many times to fit the model (with different random starting points within paramBounds
#' @return efficay, latency, precision, val (negative log likelihood), and warnings (currently disabled, don't know why)
#' @examples
#' df <-  subset(P2E2pilot,subject=="CB" & target==1 & condition==1)
#' analyzeOneConditionDF(df, 16, parameterBounds(), 1)
#'
#' @export
analyzeOneConditionDF<- function(df, numItemsInStream, paramBounds, nReplicates=3) {
  fitList<- analyzeOneCondition(df, numItemsInStream, paramBounds, nReplicates)

  #To make into dataframe, can take only the first warning
  #And I think I have to change to string from possibly being simpleWarning object, otherwise I've gotten error:
  # "cannot coerce class "c("simpleWarning", "warning", "condition")" to a data.frame"
  #Sometimes it's a list of warning, which you can't put into a data frame.
  #In that case just take the first.
  firstWarning<- fitList$warnings[1]
  #But when it's an actual warning rather than null [[1]], it will have both a msg and a call field
  if (typeof(firstWarning) == "list") {
    msg<- firstWarning$message
    call<- capture.output(  #the function call that caused the warning
      print( firstWarning[[1]]$call ) #Because print has a method that formats it nicely
    )
    msgAndCall<- paste0("message= ",msg,", call= ",call)
    fitList$warnings<- msgAndCall
  }
  else {
    fitList$warnings <- firstWarning
  }
  #toString( fitList$warnings[1] )

  asDataFrame<- data.frame(fitList)
  #asDataFrame<- data.frame(efficay=fitList[1], latency=fitList[2], precision=fit[3], val=fit$value, warnings="None")
  return( asDataFrame  )
}

