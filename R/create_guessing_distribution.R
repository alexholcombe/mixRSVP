#' Create guessing distribution
#'
#' @param minSPE Minimum possible serial position error.  ‘minSPE’ ‘maxSPE’ ‘targetSP’ ‘numItemsInStream’
#' @param maxSPE Maximum possible serial position error.
#' @param targetSP Serial position of target on each trial (array), where first item in the stream is 1, not 0.
#' @param numItemsInStream Number of items in each stream.
#' @return The guessing distribution, typically pseudo-uniform (see Goodbourn & Holcombe, 2015).
#' @examples
#' createGuessingDistribution(-12,12,c(10,10,11,11,12,12,13,13),22)
#'
#' @export
createGuessingDistribution <- function(minSPE,maxSPE,targetSP,numItemsInStream) {
  # Generate the 'pseudo-uniform' distribution, which is the expected distribution of errors if a random guess was
  # provided on every trial. This isn't an actual uniform distribution because the most extreme errors are only
  # possible on trials in which targets appear at their most extreme positions.
  # minSPE and maxSPE is redundant with targetSP and numItemsInStream but saves calculation time.

  if (minSPE > maxSPE) stop('minSPE must be less than or equal to maxSPE')
  #Check that targetSP and numItemsInStream doesn't imply SPEs that fall outside minSPE and maxSPE
  #(don't want to generate minSPE, maxSPE automatically yet because might need to use common scale with
  # different conditions or different Ss some of whom lost a trial, where conditions with fewer SPs means the
  # guessing distribution will just end up with some '0' entries, which is fine)
  maxTargetSP <- max(targetSP)
  minSPEthisData<- 1 - max(targetSP)
  maxSPEthisData<- numItemsInStream - min(targetSP)
  if (maxSPEthisData > maxSPE) stop(cat("maxSPE must be at least",maxSPEthisData,"based on the values you passed me"))
  if (minSPEthisData > minSPE) stop(cat("minSPE must be no greater than",minSPEthisData,"based on the values you passed me"))

  #For each targetSP, determine all possible SPEs and aggregate across all targetSPs to generate the guessing distribution
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
