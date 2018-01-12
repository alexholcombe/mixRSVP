context("test-analyzeonecondition.R")

#To testthat, run test_file("mixtureModeling/tests/test_analyzeOneCondition.R")
library(dplyr)

test_that("Unproblematic data is fit to right value", {
  df<-readRDS(file.path("..","exampleSubject.Rdata"))
  numItemsInStream<- 24 #because 1 to 24 all present in df$resp
  estimates<<- analyzeOneCondition(df, numItemsInStream, parameterBounds())

  expectedParamEstimates<- c(.625,.555,.798)
  discrepancy <- unlist( estimates[1:3] ) - expectedParamEstimates
  discrepancyLow <- all( abs( discrepancy ) < .1 )
  expect_that( discrepancyLow, is_true() )
})

#Test with a problematic dataset
test_that("Low-efficacy case", {

  data<- readRDS( file.path("..","alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
  library(dplyr)
  numItemsInStream<- length( data$letterSeq[1,] )
  #It seems that to work with dplyr, can't have array field like letterSeq
  data$letterSeq<- NULL
  BA22 <- data %>% dplyr::filter(subject=="BA" & target==2 & condition==2)
  #Rather low efficacy dataset
  estimates<- analyzeOneCondition(BA22,numItemsInStream,parameterBounds())

  expect_that( is.null(estimates$warnings), is_true() )

  #Check estimates are what I expect
  expectedParamEstimates<- c(.28,.07,.75)
  discrepancy <- unlist( estimates[1:3] ) - expectedParamEstimates
  discrepancyLow <- all( abs( discrepancy ) < .1 )
  expect_that( discrepancyLow, is_true() )
}
)





