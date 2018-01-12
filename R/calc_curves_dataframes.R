
#To be used with ggplot. Idea is to split dataframe into conditions,
# send it to calc_curves_dataframes to fit data if not already fit but definitely
# calculate the curves.

calc_curves_dataframes<- function(df,minSPE,maxSPE,numItemsInStream) {
  #Calculate dataframes containing the fitted curve, so can plot data and curves at same time
  #User can optionally supply estimates, otherwise need to do the fitting

  valAvailable<-FALSE
  if ( "efficacy" %in% names(df) ) { #user supplied efficacy, latency, precision
    efficacy<- df$efficacy[1]
    latency<- df$latency[1]; precision<- df$precision[1]
    if ( "val" %in% names(df) ) { #likelihood, preserve
      val <- df$val[1]
      valAvailable<-TRUE
    }
  } else {
    estimates<- analyzeOneCondition(df,numItemsInStream,parameterBounds())
    efficacy<-estimates$p1; latency<-estimates$p2; precision<-estimates$p3
    val<- estimates$val
    valAvailable<-TRUE
  }

  #create guessing distribution
  guessingDistro <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  #unitise it
  guessingDistro<- guessingDistro/sum(guessingDistro)
  #calculate points at appropriate height for this data
  guessingThis<- (1-efficacy) * guessingDistro * length(df$SPE)
  curveDfs<-data.frame(x=minSPE:maxSPE,
                     efficacy=efficacy, latency=latency, precision=precision,
                     guessingFreq=guessingThis)
  if (valAvailable) {
    curveDfs$val<- val
  }
  #Calculate Gaussian and sum
  #Need the quantized Gaussian
  grain<-1
  #print(paste0("efficacy=",efficacy,"latency=",latency,"precision=",precision))
  #print(paste0("minSPE=",minSPE,"maxSPE=",maxSPE))
  #print(paste0("df$SPE=",df$SPE))
  numObservations<- length(df$SPE)
  gaussianThis<- gaussian_scaled(efficacy,latency,precision,numObservations,minSPE,maxSPE,grain)
  #print(gaussianThis)
  curveDfs$gaussianFreq<- gaussianThis$gaussianFreq

  curveDfs$combinedFitFreq<- curveDfs$gaussianFreq + curveDfs$guessingFreq

  return( curveDfs )
}
