context("test-createGuessingDistribution.R")

minSPE<- -17; maxSPE<- 17;
numItemsInStream<-24
numTrials<-100
trialsPerSerialPosition<-10
possibleSerialPositions<- c(7,9,11,14,18)
targetSerialPositions<-rep(possibleSerialPositions,trialsPerSerialPosition)

test_that("At least it runs", {

  guessingDistro <- createGuessingDistribution(minSPE,maxSPE,targetSerialPositions,numItemsInStream)
  expect_that( length(guessingDistro), equals( maxSPE-minSPE+1 ) )

} )


test_that("Target serial positions should not correspond to serial position errors that are out of bounds", {

  possibleSerialPositions<- c(25)
  targetSerialPositions<- possibleSerialPositions

  #Test that throws error as expected
  expect_that( createGuessingDistribution(minSPE,maxSPE,targetSerialPositions,numItemsInStream) , throws_error() )

} )
