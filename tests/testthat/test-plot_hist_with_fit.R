context("test-plot_hist_with_fit.R")

test_that("executes", {
  data <- backwards2_E1 #.mat file been preprocessed into melted long dataframe
  numItemsInStream<- length( data$letterSeq[1,] )

  #To use dplyr operations, each column must be a 1d atomic vector or a list. So, can't have array fields like letterSeq
  data$letterSeq<- NULL

  #Give conditions better names than 1 and 2
  names(data)[names(data) == 'target'] <- 'stream'
  data <- data %>% mutate( stream =ifelse(stream==1, "Left","Right") )
  #mutate condition to Orientation
  names(data)[names(data) == 'condition'] <- 'orientation'
  data <- data %>% mutate( orientation =ifelse(orientation==1, "Canonical","Inverted") )

  plotContinuousGaussian<-TRUE
  annotateIt<-TRUE
  minSPE<- -17; maxSPE<- 17

  # BE,2,1
  df<- data %>% dplyr::filter(subject=="BE" & stream=="Right" & orientation=="Canonical")
  #estimates<- analyzeOneCondition(df,numItemsInStream,parameterBounds())
  #curvesDf <- dplyr::mutate(curvesDf, pvalColor = ifelse(pLRtest <= .05, "green", "red"))

  g<- plot_hist_with_fit(df,minSPE,maxSPE,df$targetSP,numItemsInStream,plotContinuousGaussian,annotateIt, FALSE)
  #show(g)
  expect_that( is.null(g), is_false() )
})
