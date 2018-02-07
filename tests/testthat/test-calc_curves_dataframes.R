context("test-calc_curves_dataframes.R")

#To testthat, run test_file("tests/testthat/test-calc_curves_dataframes.R")

#Read in some data to fit
df<-readRDS( file.path("..","exampleSubject.Rdata") )
df<- dplyr::filter(df, condition==1 & target==1)
numItemsInStream<-24
minSPE<- -17; maxSPE<- 17
curveDfs<- calc_curves_dataframes(df,minSPE,maxSPE,numItemsInStream)

test_that("Gets right value", { #right according to historical results with ancestral code
  library(dplyr)
  numItemsInStream<-24
  minSPE<- -17; maxSPE<- 17
  curveDfs<- calc_curves_dataframes(df,minSPE,maxSPE,numItemsInStream)
  expect_equal( curveDfs$x[1], -17 )
  expect_equal( round(curveDfs$efficacy[1],1), round(.839,1) )
  expect_equal( round(curveDfs$combinedFitFreq[1],2), round(.073,2) )
})
