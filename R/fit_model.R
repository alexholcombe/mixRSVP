
#way in R to return values from a function and the warnings
withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(content = val, warnings = myWarnings)
}


fitModel <- function(SPEs, minSPE, maxSPE, pseudoUniform, parameterGuess, paramBounds)
{
  #Create function that calculates log likelihood of data given particular parameter values,
  #to pass to optim
  pdf_mixture_ready_for_optim <- function(par)
  {
    p <- par[1]
    mu <- par[2]
    sigma <- par[3]

    likelihood <- likelihood_mixture(SPEs, p, mu, sigma, minSPE,maxSPE, pseudoUniform)
    # Sometimes pdf_normmixture_single returns 0. And the log of 0 is -Inf. So we add
    # 1e-8 to make the value we return finite. This allows optim() to successfully
    # optimise the function.
    return(-sum(log(likelihood + 1e-8)))
  }

  #Traverse parameter space to find parameter values that maximise the log likelihood
  testingOptimization<-FALSE
  ctrl<- list( trace=0, starttests=FALSE )
  if (testingOptimization) {
    ctrl<- list( trace=0, all.methods=TRUE, save.failures=TRUE )
  }

  #do the fit
  fit <- withWarnings(
                optimx::optimx(parameterGuess, fn= pdf_mixture_ready_for_optim,
                  method=c('L-BFGS-B'),
                  lower=paramBounds$lower, upper=paramBounds$upper,
                  control=ctrl)
           )
  return(fit)
}


# Alternatively, could use the MLE function.
# [currentEstimates, currentCIs] <- mle(theseT1Error, 'pdf', pdf_normmixture_single, 'start', parameterGuess, 'lower', parameterLowerBound, 'upper', parameterUpperBound, 'options', options)

#Before I switched to optimx, I used:
#fit <- optim(parameterGuess, pdf_normmixture_single_par, lower=parametersLowerBound, upper=parametersUpperBound,
#             control=list(trace=0), method="L-BFGS-B")
