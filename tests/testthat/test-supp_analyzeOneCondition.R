context("test-supp_analyzeOneCondition.R")
# additional tests of fitModel, intended to be used with the testthat package

#The following test is very time-consuming, so commented out for now
# test_that("Fits entire experiment worth of data fine", {
#
#   data<- readRDS( file.path("..", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
#   library(dplyr)
#   numItemsInStream<- length( data$letterSeq[1,] )
#   data<- data
#   #It seems that to work with dplyr, can't have array field like letterSeq
#   data$letterSeq<- NULL
#
#   estimates<-data %>% group_by(subject,target,condition) %>%
#               do(analyzeOneCondition(.,numItemsInStream,parameterBounds()))
#
#   #round numeric columns so easier to view
#   data.frame(lapply(estimates, function(y) if(is.numeric(y)) round(y, 2) else y))
#
#   expect_that( all(estimates$warnings == "None"), is_true() )
#
# }
# )

# #The following test is very time-consuming, so commented out for now
# test_that("Handles terrible subjects", {
#
#   data<- #.mat file been preprocessed into melted long dataframe
#     readRDS( file.path("..", "alexImportBackwardsPaper2E1excludedSs.Rdata") )
#   library(dplyr)
#   numItemsInStream<- length( data$letterSeq[1,] )
#   data<- data
#   #It seems that to work with dplyr, can't have array field like letterSeq
#   data$letterSeq<- NULL
#
#   estimates<-data %>% group_by(subject,target,condition) %>%
#     do(analyzeOneCondition(.,numItemsInStream,parameterBounds()))
#
#   #round numeric columns so easier to view
#   data.frame(lapply(estimates, function(y) if(is.numeric(y)) round(y, 2) else y))
#
#   expect_that( all(estimates$warnings == "None"), is_true() )
#
# }
# )



