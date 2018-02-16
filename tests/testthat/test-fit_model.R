context("test-fit_model.R")

#To testthat, run test_file("tests/testthat/test-fit_model.R")

test_that("Check null warning no longer occurs", {
  #Having warnings be NULL is a problem because when assign to a different dataframe, will delete the column
  #so fit_model is supposed to replace NULLs with None

  data <- backwards2_E1 #.mat file been preprocessed into melted long dataframe
  library(dplyr)
  numItemsInStream<- length( data$letterSeq[1,] )
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL

  df <- data %>% dplyr::filter(subject=="AA",condition==2,target==1)

  startingParams<- parametersGuess( parameterBounds()$lower, parameterBounds()$upper )
  possibleTargetSP<- sort(unique(df$targetSP))
  minTargetSP <- min(possibleTargetSP)
  maxTargetSP <- max(possibleTargetSP)
  minSPE <- 1 - maxTargetSP
  maxSPE <- numItemsInStream - minTargetSP
  #calculate the guessing distribution, empirically (based on actual targetSP)
  pseudoUniform <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)

  #Don't forget that fitModel is not exported, so is only accessible from within the package
  fit<- fitModel(df$SPE, minSPE, maxSPE, pseudoUniform, startingParams, parameterBounds() )

  expect_equal(fit$warnings[1], "None")
}
)

test_that("Decent estimates", {
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

  #Don't forget that fitModel is not exported, so is only accessible from within the package
  fit<- fitModel(df$SPE, minSPE, maxSPE, pseudoUniform, startingParams, parameterBounds() )
  warns<- fit$warnings
  fit<- fit$content

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


