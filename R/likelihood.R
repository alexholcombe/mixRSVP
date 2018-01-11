#library(signal) # Provides interp1 function
# interp1 <- function(x, v, xq)
# {
#   cat("x ", x, " v ", v, " xq ", xq, "\n")
#   x2 <- x[order(x)]
#   v2 <- v[order(x)]
#
#   vq <- rep(0, length(xq))
#   for (idx in 1:length(xq)) {
#     i <- 1
#     while (xq[idx] >= x2[i]) i = i + 1
#
#     # Now x[i - 1] < xq and x[i] >= xq
#     m <- (v2[i] - v2[i - 1]) / (x2[i] - x2[i - 1])
#     vq[idx] <- v2[i - 1] + m * (xq[idx] - x2[i - 1])
#   }
#
#   return(vq)
# }

#' Calculate area under a particular domain of a Gaussian distribution
#'
#'
#' @param binStart x value of bin start
#' @param binWidth bin width
#' @param latency mean of Gaussian
#' @param precision sigma of Gaussian
#' @importFrom stats dnorm pnorm runif
#' @return Area under the bin for a Gaussian distribution with the parameters mean=latency, sigma=precision
areaUnderGaussianBin<- function(binStart,binWidth,latency,precision) {
  #Calculate area under the unit curve for that bin
  area <- pnorm(binStart+binWidth,latency,precision) - pnorm(binStart,latency,precision)
  return (area)
}

areaUnderGaussianBinEachObservation<- function(SPEs, mu, sigma) {
 binWidth<-1
 binStarts <- SPEs - binWidth/2
 ans<- sapply(binStarts, binWidth, mu, sigma)
 return (ans)
}

areaUnderGaussianBinTapered<- function(binStart,binWidth,latency,precision,guessingDistribution,minSPE,maxSPE) {
  #Calculate area under the unit curve for that bin

  area<- areaUnderGaussianBin(binStart,binWidth,latency,precision)

  binCenter <- round(binStart+binWidth/2)
  #Which bin index is this? So can look up in guessingDistribution
  binIndex<- binCenter - minSPE + 1

  #Taper the Guassian area by the guessing distribution. Guessing distribution could be a large number but
  # everything will be normalized at the end
  area<- area * guessingDistribution[binIndex]

  return (area)
}

areaUnderTruncatedGaussianTapered<- function(latency,precision,guessingDistribution,minSPE,maxSPE) {
  #Calculate total area of the truncated and tapered Gaussian
  binStarts= seq(minSPE-1/2, maxSPE-1/2, 1)
  #Send each bin individually to the areaOfGaussianBin function with additional parameters
  binAreasGaussian<-sapply(binStarts, areaUnderGaussianBinTapered, 1,latency,precision,guessingDistribution,minSPE,maxSPE)

  totalArea<- sum(binAreasGaussian)

  return (totalArea)
}

areaUnderGaussianBinEachObservationTapered<- function(SPEs, mu, sigma, guessingDistribution,minSPE,maxSPE) {

 #Taper the Gaussian curve based on the guessingDistribution
 SPEsBinStarts = SPEs - .5

 #Proportional to the probability of each bin, tapered. Will need to be normalized by total tapered area
 areasTapered<- sapply(SPEsBinStarts, areaUnderGaussianBinTapered, 1, mu, sigma, guessingDistribution,minSPE,maxSPE)
 return (areasTapered)
}


likelihood_mixture <- function(x,p,mu,sigma,minSPE,maxSPE,guessingDistribution) {
  #Calculate the likelihood of each data point (each SPE observed)
  #The data is discrete SPEs, but the model is a continuous Gaussian.
  #The probability of an individual SPE is not the height of the normal at the bin's center,
  #but arguably is the average height of the curve across the whole bin, which here is captured
  #by integrating the area under the curve for the bin domain.
  #E.g., for an SPE of 0, from -.5 to +.5  . See goodbournMatlabLikelihoodVersusR.png for a picture
  gaussianComponentProbs<- areaUnderGaussianBinEachObservationTapered(x,mu,sigma,guessingDistribution,minSPE,maxSPE)
  gaussianComponentProbs<- unlist(gaussianComponentProbs) #ended up as list rather than vector with Cheryl's data
  #Now we have the probability of each observation, except they're not really probabilities
  #because the density they were based on doesn't sum to 1 because it wasn't a Gaussian
  #truncated to the domain of possible events. Plus it was tapered by the guessingDistribution

  #Make legit probabilities and compensate for  truncation of the Gaussian by summing the area of all the bins
  #and dividing that into each bin, to normalize it so that the total of all the bins =1.
  totalAreaUnderTruncatedGaussianTapered<-
              areaUnderTruncatedGaussianTapered(mu, sigma, guessingDistribution,minSPE,maxSPE)

  gaussianComponentProbs<- gaussianComponentProbs / totalAreaUnderTruncatedGaussianTapered
  #(An alternative, arguably better way to do it would be to assume that anything in the Gaussian tails
  # beyond the  bins on either end ends up in those end bins, rather than effectively redistributing the
  # tails across the whole thing.)

  #Calculate the likelihood of each observation according to the guessing component of the mixture
  #Convert SPEs to bind indices, so can look up in guessingDistribution
  distroIndices<- x - minSPE + 1 #So minSPE becomes 1
  guessingComponentProbs<- guessingDistribution[distroIndices]
  #Make it a probability
  guessingComponentProbs<- guessingComponentProbs / sum(guessingDistribution)

  #Do the mixture: deliver the probability of each observation reflecting both components.
  pGaussianComponent <- p
  pGuessingComponent <- 1-p

  gaussianContrib <- pGaussianComponent * gaussianComponentProbs
  guessingContrib <- pGuessingComponent * guessingComponentProbs

  finalProbs<- gaussianContrib + guessingContrib

  #This code from Pat looks like it deals with the situation of normResult and uniResult being
  # different shapes (a row versus a column) but I don't know how that can happen. And the conditional doesn't
  # seem to test for that
  # if (sum(length(gaussianContrib)==length(guessingContrib))!=2){ #This doesn't seem to make sense
  #   msg=paste0('Looks like one of these is the transposed shape of the other, str(gaussianContrib)=',
  #              str(gaussianContrib),' str(guessingContrib)=',str(guessingContrib) )
  #   warning(msg)
  #   finalProbs <- gaussianContrib + t(guessingContrib)
  # }
  return(finalProbs)
}


likelihood_mixture_MATLAB <- function(x,p,mu,sigma,minSPE,maxSPE,guessingDistribution){
  #Calculate the likelihood of each data point (each SPE observed) according to the mixture model.
  #Port of Patrick's MATLAB way of doing it. His function originally called pdf_Mixture_Single to contrast with Dual in AB analysis

    xDomain <- minSPE:maxSPE

    #First we calculate normalizing factors.
    #Ideally we'd be working with probability density functions, which are guaranteed to sum to 1.
    #Instead we're using a guessingDistribution that was not programmed to be the unit density,
    # and we're using the Gaussian curve which is a density function but we're using it in a weird way:
    # First, it's truncated by xDomain and second, it's discretized and instead of using the area
    # under the Gaussian for the whole discrete bin, instead the height of the density is used.,
    # so even if it wasn't truncated, it wouldn't add up to 1.
    pseudo_normal <- dnorm(xDomain,mu,sigma)*guessingDistribution
    #cat("pseudo_normal ", pseudo_normal, "\n")

    #normalising factors
    normFactor_uniform <- sum(guessingDistribution)
    normFactor_normal <- sum(pseudo_normal)

    #How could these ever be zero? If mu was out of range?
    if (normFactor_uniform  == 0 || is.nan(normFactor_uniform)){
        normFactor_uniform <- 10^-8
    }

    if (normFactor_normal == 0 || is.nan(normFactor_normal)){
        normFactor_normal <- 10^-8
    }

    #For all SPEs, determine the height of the guessingDistribution
    #Use interpolate in case there is a rounding problem? Alex doesn't understand why interp used
    uniResultTemp <- signal::interp1(xDomain, guessingDistribution, x)
    uniResultTemp[is.na(uniResultTemp)] <- 0
    #I guess Pat called it uni because this is like the one-component model of only guessing occurring?

    #Multiply Gaussian density by guessing density
    #I'm thinking Pat did this to window the Gaussian by the tapering of possible SPEs. This isn't
    # quite the right thing to do but is probably close enough because the tails are so light in those
    # extreme (affected by tapering) SPEs.
    normResultTemp <- dnorm(x,mu,sigma) * uniResultTemp

    #I think this code turns each curve height at each SPE into a legit probability by dividing by
    # the total of the curve heights.
    uniResultTemp <- uniResultTemp/normFactor_uniform
    normResultTemp <- normResultTemp/normFactor_normal

    #Do the mixture: deliver the probability of each observation reflecting both components.
    propNorm <- p
    propUniform <- 1-p

    normResult <- propNorm * normResultTemp
    uniResult <-  propUniform * uniResultTemp

    #This code looks like it deals with the situation of normResult and uniResult being
    # different shapes (a row versus a column) but I don't know how that can happen.
    if (sum(length(normResult)==length(uniResult))==2){
        result <- normResult+uniResult
    } else {
        result <- normResult+t(uniResult)
    }

    #xIndex = x-min(xDomain)+1;
    #results = tempResult(xIndex);
    # cat("tempResult ", tempResult, "\n")
    #tempResult <- tempResult[1]
    return(result)
}


likelihoodOneConditionGivenParams<- function(df, numItemsInStream, params) {
  #Note that the likelihood depends on the number of observations. So it'd be unfair to compare the
  # fit across different datasets. Could divide by the number of observations to calculate
  # the average likelihood but probably probabilities don't work that way.
  possibleTargetSP<- sort(unique(df$targetSP))
  minTargetSP <- min(possibleTargetSP)
  maxTargetSP <- max(possibleTargetSP)
  minSPE <- 1 - maxTargetSP
  maxSPE <- numItemsInStream - minTargetSP
  #calculate the guessing distribution, empirically (based on actual targetSP)
  pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  p<- params[[1]]
  mu<- params[[2]]
  sigma<- params[[3]]
  likelihoodEachObservation <- likelihood_mixture(df$SPE, p, mu, sigma, minSPE,maxSPE, pseudoUniform)
  # Sometimes pdf_normmixture_single returns 0. And the log of 0 is -Inf. So we add
  # 1e-8 to make the value we return finite. This allows optim() to successfully
  # optimise the function.
  return(-sum(log(likelihoodEachObservation + 1e-8)))
}

################
likelihoodOneConditionForDplyr<- function(df,numItemsInStream) {
  params<- df[1,c("efficacy","latency","precision")]
  l<- likelihoodOneConditionGivenParams(df, numItemsInStream, params)
  return(data.frame(val=l))
}

