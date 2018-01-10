#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
createGuessingDistribution <- function(minSPE,maxSPE,targetSP,numItemsInStream) {
  # Generate the 'pseudo-uniform' distribution, which is the expected distribution of errors if a random guess was
  # provided on every trial. This isn't an actual uniform distribution because the most extreme errors are only
  # possible on trials in which targets appear at their most extreme positions.
  # minSPE and maxSPE is redundant with targetSP and numItemsInStream but saves calculation time.
  xDomain<- minSPE:maxSPE
  numPossibleSPEs<- maxSPE-minSPE+1
  pseudoUniform <- matrix(xDomain, ncol=2,nrow=numPossibleSPEs, byrow=FALSE)
  #first column will be SPE. Second column will be expected frequency of that SPE from guessing
  pseudoUniform[,2] <-0
  # Cycle through each possible T1 position.
  # Will use empirical T1 positions (doesn't assume was perfectly counterbalanced), because that yields the objective expectation
  # if you simply guess on every trial.
  for (i in 1:length( targetSP )) {
    #for (i in 1:length(possibleT1Pos)){ #theoretical  (assumes each possible position occurred equally often)

    thisPos <- targetSP[i]   #For example, the first position number might be the 7th position in the stream.

    # Add to the pseudo-uniform distribution one unit for every
    # possible error given that T1 position.

    #what's the min and max SPE corresponding to this T1 position
    minSPEthis<- thisPos - numItemsInStream
    maxSPEthis<- thisPos - 1
    #where does this fit in to the entire range of possible SPEs
    minSPEthisRel<- minSPEthis - minSPE
    maxSPEthisRel<- maxSPEthis - minSPE

    minThis <- 1-thisPos-minSPE+1
    maxThis <- numItemsInStream-thisPos-minSPE+1
    pseudoUniform[minThis:maxThis,2] = pseudoUniform[minThis:maxThis,2] + 1
  }
  #Only want second column, not labels
  pseudoUniform<- pseudoUniform[,2]

  #Pat padded with extra zero for some reason. And only want second column, not labels
  pad<-FALSE
  if (pad) {
    pseudoUniform<- c(0, pseudoUniform, 0)
  }
  return(pseudoUniform)
  #length(pseudoUniform)#pseudoUniform is 37 long, padded or 35 unpadded
  #Needs to match up with
  #length(dnorm(xDomain,muGuess,sigmaGuess)) #35 long, which makes sense. -17->17

}
