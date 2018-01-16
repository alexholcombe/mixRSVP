#Calculate the underlying continuous Gaussian, not discretized. But how high should it be?

#Given a dataset of 100 points. Take the Gaussian density, area under the curve is 1,
#and make the area under the curve = 100?
#But wait a second - the curve goes forever, but I'm only showing -17 to 17.
#No, but each of those 100 points is actually a grainsize portion of the curve.
#So, calculate the area of the Gaussian density for each bin. Then multiply by a normalizing
#factor so that the total area of all the bins equals the number of datapoints.
#This is neglecting that we're talking about a truncated Gaussian, and
#  possiby tapered by frequency of possible SPEs

areaOfGaussianBin<- function(binStart,binWidth,latency,precision) {
  #Calculate area under the unit curve for that bin
  area <- pnorm(binStart+binWidth,latency,precision) - pnorm(binStart,latency,precision)
  return (area)
}

#' Calculate the mixture model's Gaussian, scaled to the number of datapoints in the data
#'
#' Used in plot_hist_with_fit etc.
#'
#' @export
#'
#' @examples
#' df <-  subset(P2E2pilot,subject=="CB" & target==1 & condition==1)
#' calc_curves_dataframes(df, -11, 11, 16)
#'
gaussian_scaled<- function(efficacy,latency,precision,numObservations,minSPE,maxSPE,grain) {
  #At each minSPE:grain:maxSPE value, calculate Gaussian at
  #  height appropriate for plotting with the histogram of the SPE data

  #Calculate area under the unit Gaussian curve for each bin

  #Calculate beginning x value of each bin, e.g. -0.5 for 0 if grain =1
  binStarts= seq(minSPE-grain/2, maxSPE-grain/2, grain)
  #Calculate area under the unit Gaussian for each bin. This corresponds
  #Send each SPE individually to the areaOfGaussianBin function with additional parameters
  binAreasGaussian<- sapply(binStarts, areaOfGaussianBin,   grain,latency,precision)

  #Now we have the probability of each bin, but they don't sum to 1 because it wasn't at truncated Gaussian
  #We can now accomplish truncation of the Gaussian by summing the area of all the bins and dividing
  #that into each bin, to normalize it so that the total of all the bins =1.
  probEachBin<- binAreasGaussian / sum(binAreasGaussian)
  #An alternative, arguably better way to do it would be to assume that anything in the Gaussian tails
  # beyond the  bins on either end ends up in those end bins.

  #Now there's the matter of the actual dataset.
  #The area under the histogram does not sum to 1, it instead sums to the number of trials.
  #So we need to achieve the same thing for our theoretical Gaussian curve, except that
  #our Gaussian curve is responsibe for only efficacy proportion of the trials.

  #numObservations reflects grain of 1. But if using finer grain to plot, need to scale up
  numTheoreticalObservations <- numObservations * 1/grain

  gaussianThis<- probEachBin * numTheoreticalObservations * efficacy

  #Slide it into a dataframe with the bins indicated
  gaussianThis<-data.frame(x=seq(minSPE,maxSPE,grain), gaussianFreq=gaussianThis)

  #Note: not tapered

  return(gaussianThis)
}


#Below is mistaken old way that used plain density without integrating the area of each bin
gaussianScaledForDataOld<- function(efficacy,latency,precision,numObservations,minSPE,maxSPE,grain) {
  #Return the correct heights of the Gaussian at each minSPE:grain:maxSPE at
  # the right height for plotting with the histogram of the SPE data
  domain<-seq(minSPE,maxSPE,grain)
  gaussianThis<- dnorm(domain,latency,precision)
  #Calculate points at appropriate height for this data
  #print(paste0("lengthSPE=",length(SPE)))
  gaussianThis<- gaussianThis * efficacy * numObservations
  gaussianThis<-data.frame(x=domain, gaussianFreq=gaussianThis)
  return(gaussianThis)
}


#Need to calculate Gaussian heights at this finer grain for each condition
#Each call to gaussianScaledForData returns a larger dataframe than was sent.
# To do that with dplyr, need to be part of dataframe

gaussianScaledFromDataframe<- function(df,minSPE,maxSPE,grain) {
  #Should be sent a one-line dataframe with efficacy,latency,precision
  #Want to expand that into entire curve, with many different SPE values
  #nPerCond is number of observations per condition
  stopifnot( "nPerCond" %in% colnames(df) )
  curve<- gaussian_scaled(df$efficacy,df$latency,df$precision,df$nPerCond,minSPE,maxSPE,grain)

  #To merge the parameter estimates with every line of this set of x,gassianFreq points for the curve:
  # merge(iris, data.frame(time=1:10), by=NULL) https://stackoverflow.com/questions/11693599/alternative-to-expand-grid-for-data-frames
  withParams<- merge(df, curve, by=NULL)
  return(withParams)
}

# Example call:
#gaussianFine<- estimates_M %>% group_by_at(.vars = group_vars) %>% do(
#  gaussianScaledFromDataframe(.,minSPE,maxSPE,grain) )
