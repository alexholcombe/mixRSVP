context("test-likelihood.R")

test_that("likelihood guessing check 2 cases", {

  #From an actual experiment
  df<-data.frame(
       targetSP= scan( textConnection("16 9 17 14 7 11 7 8 11 12 9 14 11 17 18 8 16 15 16 7 16 18 10 16 12 13 17 9 8 16 17 17 13 8 7 18 13 16 8 17 18 9 18 16 9 11 18 17 14 10 17 7 17 18 14 10 18 17 12 15 11 17 8 11 9 13 17 14 13 8 10 9 11 13 7 7 7 7 9 10 10 13 8 10 7 8 14 16 7 9 8 15 11 17 14 15 14 16 14 14"))
       )
  numItemsInStream <- 24
  possibleTargetSP<- sort(unique(df$targetSP))
  minTargetSP <- min(possibleTargetSP)
  maxTargetSP <- max(possibleTargetSP)
  minSPE <- 1 - maxTargetSP
  maxSPE <- numItemsInStream - minTargetSP

  guessingDistribution <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  nTrials<- length(df$targetSP)

  set.seed(3) # Reproducibility
  #Use the actual guessing distribution as the SPEs by sampling without replacement
  guesses<- sample(minSPE:maxSPE,nTrials,replace=TRUE,prob=guessingDistribution) #https://stackoverflow.com/a/46590082/302378
  #hist(guesses, breaks=seq(minSPE,maxSPE) )
  df$SPE <- guesses
  negLogLik<- likelihood_guessing(df,numItemsInStream)
  expect_equal(negLogLik,341.0288,tolerance=1e-3)


  #A real series of SPEs with a reasonable efficacy,  from subset(backwards2_E1, subject=="AA" & condition==2 & target==1)
  df$SPE <- scan( textConnection('1 9 -3 -7 8 1 17 2 -4 -3 1 1 2 1 1 6 -8 2 -7 1 4 -5 1 1 0 -7 1 0 1 1 1 2 6 11 0 0 0 -1 1 -8 -2 1 -12 1 1 -10 -13 -10 1 -1 1 3 -3 0 -2 1 3 1 1 -11 0 -3 -4 -3 1 1 2 1 -9 1 1 -3 -1 -1 5 9 -3 1 1 11 -7 -6 15 1 0 -1 2 -8 1 13 2 -3 -3 1 -13 -1 -11 1 6 1') )
  #hist(df$SPE, breaks=seq(minSPE,maxSPE) )
  negLogLik<- likelihood_guessing(df,numItemsInStream)
  expect_equal( negLogLik, 330.0893,  tolerance=1e-3 )

})
