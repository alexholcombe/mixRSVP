context("test-calc_curves_dataframe.R")

#To testthat, run test_file("tests/testthat/test-calc_curves_dataframe.R")

#Read in some data to fit
df<-readRDS( file.path("..","exampleSubject.Rdata") )
#df<-readRDS( file.path("tests","exampleSubject.Rdata") ) #if running from top workspace outside of testthat
dg<- subset(df, condition==1 & target==1)

test_that("Gets right value", { #right according to historical results with ancestral code
  numItemsInStream<-24
  minSPE<- -17; maxSPE<- 17
  curveDf<- calc_curves_dataframe(dg,minSPE,maxSPE,numItemsInStream)

  expect_equal( curveDf$x[1], -17 )
  expect_equal( round(curveDf$efficacy[1],1), round(.839,1) )
  expect_equal( round(curveDf$combinedFitFreq[1],2), round(.073,2) )
})

test_that("Performs with dplyr split-apply-recombining into dataframe", {
  library(dplyr)
  numItemsInStream<-24
  minSPE<- -17; maxSPE<- 17

  condtnVariableNames<-c("condition","target")
  curves<- df %>% group_by_at(.vars = condtnVariableNames) %>%
    do(calc_curves_dataframe(.,minSPE,maxSPE,numItemsInStream))

  expect_silent(curves)
})
