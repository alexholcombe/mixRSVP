context("test-fit_model.R")

#Try a dataset that can give grad.default pesky fitting error like
#To testthat, run test_file("tests/testthat/test-fit_model.R")

test_that("grad default error", {
  #Trying to get on top of occasional believed-harmless error in function fit, that looks like:
  #Error in grad.default(ufn, ans$par, ...) : function returns NA at 1.4434927002511e-050.000135401581392880.000100001 distance from x.,
  #Explained by Nash here: http://r.789695.n4.nabble.com/Re-optim-bbmle-function-returns-NA-at-td4673616.html
  data <- backwards2_E1
  data$letterSeq<-NULL
  dataAAinvertedLeft <- data %>% dplyr::filter(subject=="AA",condition==2,target==1)
  analyzeOneCondition(dataSmall,numItemsInStream,parameterBounds(), nReplicates=9)
})


test_that("Decent estimates", {
  print(getwd())
  df<-readRDS( file.path("..","exampleSubject.Rdata") ) #because dir will be tests/testthat
  #library(dplyr)
  df<- dplyr::filter(df, condition==1 & target==1)
  numItemsInStream<-24

  startingParams<- parametersGuess( parameterBounds()$lower, parameterBounds()$upper )

  possibleTargetSP<- sort(unique(df$targetSP))
  minTargetSP <- min(possibleTargetSP)
  maxTargetSP <- max(possibleTargetSP)
  minSPE <- 1 - maxTargetSP
  maxSPE <- numItemsInStream - minTargetSP
  #calculate the guessing distribution, empirically (based on actual targetSP)
  pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)

  fit<- fitModel(df$SPE, minSPE, maxSPE, pseudoUniform, startingParams, parameterBounds() )
  fit<- fit$content
  warns<- fit$warnings

  #Check that standard fit method gives decent results
  expectedParamEstimates<- c(.84,.48,.99) # c(.37,1.2,.017)  #from L-BFGS-B
  LBFGSBparams<-  fit["L-BFGS-B",]
  discrepancy <- LBFGSBparams[1:3] - expectedParamEstimates
  discrepancyLow <- all( abs( discrepancy ) < .1 )
  expect_that( discrepancyLow, is_true() )
  #expect_that( roots, equals(-3000.01, tolerance  = 0.1) )

  # A kkt1 of True means that the final gradient was close to 0 (the optimizer found an extremum),
  #a kkt2 of True means that the Hessian is positive definite (it's a minimum). Both should be True.
  expect_that( fit["L-BFGS-B","kkt1"], is_true() ) #means that the final gradient was close to 0 (the optimizer found an extremum)
}
)


