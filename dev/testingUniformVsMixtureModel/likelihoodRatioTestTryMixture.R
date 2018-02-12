

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



#sample a lot of trials from the uniform distribution. Note that this is not the pseudoUniform
n=nrow(df)
SPEs<- sample(minSPE:maxSPE, size=n, replace=T)
df$SPE<-SPEs

set.seed(1) # Reproducibility

likelihood_guessing(df,numItemsInStream)


guessingDistribution <- createGuessingDistribution(minSPE,maxSPE,targetSP,numItemsInStream)


#Based on https://stats.stackexchange.com/questions/155474/r-why-does-lrtest-not-match-anovatest-lrt

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
pchisq(as.numeric(like.diff) * 2, df=df.diff, lower.tail=F)

# Anova
anova(base, full, test="LRT")

# lrtest
library(lmtest)
lrtest(base, full)
