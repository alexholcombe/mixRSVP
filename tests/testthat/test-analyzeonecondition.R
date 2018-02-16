context("test-analyzeOneCondition.R")

#To testthat, run test_file("tests/testthat/test-analyzeOneCondition.R") or RStudio:Build:Test Package
library(dplyr)

test_that("Unproblematic data is fit to right value", {
  df<-readRDS(file.path("..","exampleSubject.Rdata"))
  numItemsInStream<- 24 #because 1 to 24 all present in df$resp]
  bounds <- parameterBounds()
  estimates <- analyzeOneCondition(df, numItemsInStream, bounds)

  expectedParamEstimates<- c(.625,.555,.798)
  discrepancy <- unlist( estimates[1:3] ) - expectedParamEstimates
  discrepancyLow <- all( abs( discrepancy ) < .1 )
  expect_that( discrepancyLow, is_true() )

  expectedVal<- 996.72 #negLogLikelihood
  expect_equal(expectedVal, estimates$val, tolerance=.1)
})

#Test with a problematic dataset
test_that("Low-efficacy case", {

  data <- backwards2_E1 #.mat file been preprocessed into melted long dataframe
  library(dplyr)
  numItemsInStream<- length( data$letterSeq[1,] )
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL
  BA22 <- data %>% dplyr::filter(subject=="BA" & target==2 & condition==2)
  #Rather low efficacy dataset
  set.seed(1) # Reproducibility
  estimates<- analyzeOneCondition(BA22,numItemsInStream,parameterBounds())

  expect_equal( estimates$warnings[1], "None" )

  #Check estimates are what I expect
  expectedParamEstimates<- c(.28,.07,.75)
  discrepancy <- unlist( estimates[1:3] ) - expectedParamEstimates
  discrepancyLow <- all( abs( discrepancy ) < .1 )
  expect_true( discrepancyLow )

  expect_lt( estimates$pLRtest, 1e-07 ) #efficacy only .28 but still passes likelihood ratio test with flying colors
}
)

#Test can combine result of fitting multiple cases into a dataframe
test_that("can combine results of analyzeOneConditionDf into dataframe", {

  #If works, won't ever get this error:
  # cannot coerce class "c("simpleWarning", "warning", "condition")" to a data.frame

  data <- backwards2_E1 #.mat files been preprocessed into melted long dataframe
  numItemsInStream<- length( data$letterSeq[1,] )
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL
  condtnVariableNames <- c("target","condition","subject")

  #But need a better case where warnings returned
  BA2 <- data %>% dplyr::filter(subject=="BA" & target==2) # & condition==2)

  estimates<- BA2 %>%
    group_by_(.dots = condtnVariableNames) %>%  #.dots needed when you have a variable containing multiple factor names
    do(  analyzeOneConditionDF(.,numItemsInStream,parameterBounds(), nReplicates=1)  )

  expect_length(estimates,10) #basically just checking it works
})


# test_that("grad default error", {
#   #Trying to get on top of occasional believed-harmless error in function fit, that looks like:
#   #Error in grad.default(ufn, ans$par, ...) : function returns NA at 1.4434927002511e-050.000135401581392880.000100001 distance from x.,
#   #Explained by Nash here: http://r.789695.n4.nabble.com/Re-optim-bbmle-function-returns-NA-at-td4673616.html
#   data <- backwards2_E1
#   numItemsInStream<- length( data$letterSeq[1,] )
#   data$letterSeq<-NULL #Have to do this because dplyr can't handle array fields
#   dataAAinvertedLeft <- data %>% dplyr::filter(subject=="AA",condition==2,target==1)
#   analyzeOneCondition(dataAAinvertedLeft,numItemsInStream,parameterBounds(), nReplicates=9)
# })



