context("test-model-comparison.R")

#To test manually, test_file("tests/testthat/test-model-comparison.R")

test_that("simplest ground truth", {
  #Not testing any functions in model-comparison,
  #instead just testing my understanding of the chi-square likelihood ratio test that programmed  into
  #likelihoodRatioTest

  #Based on https://stats.stackexchange.com/questions/155474/r-why-does-lrtest-not-match-anovatest-lrt

  set.seed(1) # Reproducibility
  n=100
  #y = runif(n, min=-1, max=1)
  x= (1:n) / n
  b =  .1 # .000000001 #intercept
  noise = runif(n, min=-.5, max=.5)

  # Make y dependent on the other two variables
  y = b + 1*x + noise # ifelse(a==1, 0.25, 0)
  mydata = data.frame(y,a,b)

  plot(x,y)

  # Models
  base = lm(y ~ 0 + x, data=mydata)  #regression with no intercept
  full = lm(y ~ x, data=mydata)      #regression with intercept

  # Homebrew log-likelihood test
  like.diff = logLik(full) - logLik(base) #you can see here that it's not the negative log likelhhiood, rather the
  #positive log likelihood is used. And it's a ratio because it's a simplification of logs.
  #df.diff = full$df.residual - base$df.residual
  df.diff = base$df.residual - full$df.residual #this is not the number of free parameters, rather
  #it reflects the number of points too but those cancel out, leaving only the diff of number of parameters
  #Why does he use base - full instead of full - base

  #Lower p-value means the base model is less likely.
  p<- pchisq(as.numeric(like.diff) * 2, df=df.diff, lower.tail=F)

  #.027 is the p-value I got when first programming it
  expect_equal( p, .027, tolerance = .001)

  #When df.diff is greater (more parameters in the full model), p-value should be higher
  #because full model has more parameters so should be penalized
  df.diff = df.diff + 1
  p2<- pchisq(as.numeric(like.diff) * 2, df=df.diff, lower.tail=F)
  expect_true( p2 - p > 0)
} )

test_that("fake guessing observer fails likelihood ratio test", {

  #set up a fake observer who guesses all the time
  df <- backwards2_E1
  numItemsInStream<- length( df$letterSeq[1,] )
  df <- subset(df, subject=="AA" & condition==2 & target==1)
  possibleTargetSP<- sort(unique(df$targetSP))
  minTargetSP <- min(possibleTargetSP)
  maxTargetSP <- max(possibleTargetSP)
  minSPE <- 1 - maxTargetSP
  maxSPE <- numItemsInStream - minTargetSP
  bounds <- parameterBounds()

  set.seed(1) # Reproducibility


  #create guesser
  guessingDistribution <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  #Use the actual guessing distribution as the SPEs by sampling without replacement
  guesses<- sample(minSPE:maxSPE,nTrials,replace=TRUE,prob=guessingDistribution) #https://stackoverflow.com/a/46590082/302378
  #hist(guesses, breaks=seq(minSPE,maxSPE) )
  dGuesser <- df
  dGuesser$SPE <- guesses

  nGuessing<- -logLikGuessing(dGuesser,numItemsInStream)

  #fit <- fitOneCondition(dGuesser, numItemsInStream, bounds, nReplicates = 1).
  #Above function fit resulted in effficay 1e-05, -2.86, 2.92, val=343.5077
  nMixture<- 343.5077
  #Check whether mixture fit significantly better than guessing distribution
                      #send log likelihood, not negative log likelihood
  p <- likelihoodRatioTest(-nMixture,-nGuessing,3)

  #p-value should be essentially 1 because the data is from the reduced model (guessing)

  expect_equal(p, 1, tolerance=1e-03)

})


test_that("hybrid observer", {

  #set up a fake observer who guesses all the time
  df <- backwards2_E1
  numItemsInStream<- length( df$letterSeq[1,] )
  df <- subset(df, subject=="AA" & condition==2 & target==1)
  possibleTargetSP<- sort(unique(df$targetSP))
  minTargetSP <- min(possibleTargetSP)
  maxTargetSP <- max(possibleTargetSP)
  minSPE <- 1 - maxTargetSP
  maxSPE <- numItemsInStream - minTargetSP
  bounds <- parameterBounds()
  set.seed(1) # Reproducibility

  #create guesser
  guessingDistribution <- createGuessingDistribution(minSPE,maxSPE,df$targetSP,numItemsInStream)
  #Use the actual guessing distribution as the SPEs by sampling without replacement
  guesses<- sample(minSPE:maxSPE,nTrials,replace=TRUE,prob=guessingDistribution) #https://stackoverflow.com/a/46590082/302378
  #hist(guesses, breaks=seq(minSPE,maxSPE) )
  guesser <- df
  guesser$SPE <- guesses

  #Create observer with lots of guesses
  loser <- rbind(guesser)#,df)
  #hist(loser$SPE, breaks=seq(minSPE,maxSPE,by=1))

  nGuessing<- -logLikGuessing(loser,numItemsInStream)

  fit <- fitOneCondition(loser, numItemsInStream, bounds, nReplicates = 1)
  #efficacy around .18
  nMixture<- fit$val

  #Check whether mixture fit significantly better than guessing distribution
  #and return both the statistical test and the likelihood difference

  p <- likelihoodRatioTest(-nMixture,-nGuessing,3)
  #this example fails to reject the null, as it should because participant is pure guesser
  #p-value should be essentially 1 because the data is from the reduced model (guessing)
  expect_equal(p, .83, tolerance=1e-02)

  #ANOTHER TEST, of data that is not 100% guesses

  #Create observer who is not 100% guesses
  loser <- rbind(guesser,df[1:10,]) #rbind(guesser[1:1,],df)
  #hist(loser$SPE, breaks=seq(minSPE,maxSPE,by=1))

  nGuessing<- -logLikGuessing(loser,numItemsInStream)

  fit <- fitOneCondition(loser, numItemsInStream, bounds, nReplicates = 1)
  #efficacy around .18
  nMixture<- fit$val

  #Check whether mixture fit significantly better than guessing distribution
  p <- likelihoodRatioTest(-nMixture,-nGuessing,3)

  #Notice that including just 10 observations from a not particularly good participant, of 110 trials total
  expect_equal(p, .07766, tolerance=1e-04)

})


