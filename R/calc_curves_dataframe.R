#' Helper to create x,ys of fitted curves so can plot
#'
#' To be used with ggplot. Idea is to split dataframe into conditions,
#' send it to calc_curves_dataframes to fit data if not already fit and either way
#' calculate the curves.
#'
#' @param df the data as a dataframe. Must contain SPE and targetSP
#' @param minSPE smallest possible serial position error
#' @param maxSPE largest possible serial position error
#' @param numItemsInStream number of items in the RSVP stream (the stimulus)
#'
#' @export
#'
#' @examples
#' df <-  subset(P2E2pilot,subject=="CB" & target==1 & condition==1)
#' calc_curves_dataframe(df, -11, 11, 16)
#'
calc_curves_dataframe<- function(df,minSPE,maxSPE,numItemsInStream) {
  #Create dataframes containing the fitted curve, so can plot data and curves at same time
  #User can optionally supply estimates, otherwise need to do the fitting

  estimateAvailable<-FALSE
  if ( "efficacy" %in% names(df) ) { #user supplied efficacy, latency, precision, so don't need to fit
    if (length(unique(df$efficacy[1])) > 1) {
      warning( "Seems you sent data for multiple conditions - using the efficacy of the first row")
    }
    efficacy<- df$efficacy[1]
    latency<- df$latency[1]; precision<- df$precision[1]
  } else {
    estimate<- analyzeOneCondition(df,numItemsInStream,parameterBounds())
    efficacy<-estimate$efficacy; latency<-estimate$latency; precision<-estimate$precision
    estimateAvailable<-TRUE
  }

  #create guessing distribution
  guessingDistro <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  #unitise it
  guessingDistro<- guessingDistro/sum(guessingDistro)
  #calculate points at appropriate height for this data
  guessingThis<- (1-efficacy) * guessingDistro * length(df$SPE)
  curveDf<-data.frame(x=minSPE:maxSPE,
                       efficacy=efficacy, latency=latency, precision=precision,
                       guessingFreq=guessingThis)

  if ( "val" %in% names(df) ) {
    curveDf$val <- df$val[1]
  } else if (estimateAvailable) {
    curveDf$val <- estimate$val
  }
  if ( "warnings" %in% names(df) ) {
    curveDf$warnings <- df$warnings[1]
    print(paste('assigning warning from df',df$warnings[1],'which is type',typeof(df$warnings[1])))
  } else if (estimateAvailable) {
    #Sometimes it's a list of warning, which you can't put into a data frame.
    #In that case just take the first.
    theWarning<- estimate$warnings[1]
    #But when it's an actual warning rather than null [[1]], it will have both a msg and a call field
    curveDf$warnings <- theWarning
    if (typeof(theWarning) == "list") {
      msg<- theWarning$message
      call<- capture.output(  #the function call that caused the warning
        print( theWarning[[1]]$call ) #Because print has a method that formats it nicely
      )
      msgAndCall<- paste0("message= ",msg,", call= ",call)
      curveDf$warnings<- msgAndCall
    }
    else {
      curveDf$warnings <- theWarning
    }
    print(paste('assigning warning from estimate',estimate$warnings[1],'which is type',typeof(estimate$warnings[1])))
    if (typeof(estimate$warnings[1]) == "list") {
      ww<<-estimate$warnings[1]
    }
  }
  if ( "pLRtest" %in% names(df) ) {
    curveDf$pLRtest <- df$pLRtest[1]
  } else if (estimateAvailable) {
    curveDf$pLRtest <- estimate$pLRtest
  }
  if ( "valGuessing" %in% names(df) ) {
    curveDf$valGuessing <- df$valGuessing[1]
  } else if (estimateAvailable) {
    curveDf$valGuessing <- estimate$valGuessing
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
  curveDf$gaussianFreq<- gaussianThis$gaussianFreq

  curveDf$combinedFitFreq<- curveDf$gaussianFreq + curveDf$guessingFreq

  return( curveDf )
}

