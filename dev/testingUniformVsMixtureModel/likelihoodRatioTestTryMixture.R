

#set up a fake observer who guesses all the time
df <- backwards2_E1
numItemsInStream<- length( df$letterSeq[1,] )
#data$letterSeq<-NULL #Have to do this because dplyr can't handle array fields
df <- subset(df, subject=="AA" & condition==2 & target==1)
possibleTargetSP<- sort(unique(df$targetSP))
minTargetSP <- min(possibleTargetSP)
maxTargetSP <- max(possibleTargetSP)
minSPE <- 1 - maxTargetSP
maxSPE <- numItemsInStream - minTargetSP

bounds <- parameterBounds()
bestEstimate <- fitOneCondition(df, numItemsInStream, bounds, nReplicates = 3)
#Check whether this fits significantly better than guessing distribution
#and return both the statistical test and the likelihood difference
logLikGuessing <- -1* likelihood_guessing(df, numItemsInStream)
logLikMixture <- -1* bestEstimate$val

# Homebrew log-likelihood test
like.diff = logLikMixture - logLikGuessing #you can see here that it's not the negative log likelhhiood, rather the
#positive log likelihood is used. And it's a ratio because it's a simplification of logs.
#df.diff = full$df.residual - base$df.residual
df.diff = 3  #this is not the number of free parameters, rather
#it reflects the number of points too but those cancel out, leaving only the diff of number of parameters
#Why does he use base - full instead of full - base

#Lower p-value means the base model is less likely.
pchisq(as.numeric(like.diff) * 2, df=df.diff, lower.tail=F)


#Based on https://stats.stackexchange.com/questions/155474/r-why-does-lrtest-not-match-anovatest-lrt


