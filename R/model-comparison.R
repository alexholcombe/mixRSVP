likelihoodRatioTest <- function(logLikFull,logLikBase, n.params.diff)
{
  #The likelihood ratio test requires nested models.
  #A null hypothesis is often stated by saying the parameter has the value
  #Usually a p-value is p(d|H) so I'm not clear on whether this delivers the same thing. Maybe it's the nested
  #property that makes it work, so that the complement is the other universe of possibilities. Right,
  #because a p-value works by integrating over all extreme values.. but you don't know what an extreme value is
  #unless you define the alternatie hypothesis?

  #check that likelihoods are negative
  if (logLikFull > 1 || logLikBase > 1){
    stop( cat("You sent me a log likelihood with a value greater than 1. Impossible.",
             "You probably accidentally sent a negative log likelihood.") )
  }
  # Homebrew log-likelihood test
  like.diff = logLikFull - logLikBase #you can see here that it's not the negative log likelhhiood, rather the
  #positive log likelihood is used. And it's a ratio because it's a simplification of logs.
  #df.diff = full$df.residual - base$df.residual
  df.diff = 3  #this is not the number of free parameters, rather
  #it reflects the number of points too but those cancel out, leaving only the diff of number of parameters
  #Why does he use base - full instead of full - base

  #Lower p-value means the base model is less likely.
  probDataGivenNull <- pchisq(as.numeric(like.diff) * 2, df=n.params.diff, lower.tail=F)
  return (probDataGivenNull)
}
