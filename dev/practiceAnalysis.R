P2E2pilot
numItemsInStream <- dim(P2E2pilot$letterSeq)[2] #letterSeq is the letters of each trial's stream,
   #the length of the second dimension is how many letters are in each trial'
analyzeOneCondition(P2E2pilot, numItemsInStream, parameterBounds(), nReplicates=3)
#Error in createGuessingDistribution(minSPE, maxSPE, df$targetSP, numItemsInStream) :
#unused arguments (df$targetSP, numItemsInStream)
