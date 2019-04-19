#' Generate guess (random starting values) for each parameter.
#'
#' @param lowerBounds Numeric array of length 3.
#' @param upperBounds Numeric array of length 3.
#'
#' @details Simply generate random numbers between the corresponding lower and upper bounds, return it in guess
#'
#' @export
#'
#' @examples
#' parametersGuess(parameterBounds()$lower, parameterBounds()$upper)
parametersGuess<- function( lowerBounds, upperBounds ) {
  guess<- rep(0,3)
  for (i in 1:length(guess)) {
    #random value between min and max possible value
    guess[i] <- runif(n=1, min=lowerBounds[i], max=upperBounds[i] )
  }
  return (guess)
}

#' Set parameter bounds.
#'
#' @details Pat found bounds were needed to
#' prevent over-fitting to blips in the distributions. These
#' values are about right in most cases, but might need tweaking if
#' e.g. you were analysing data with an unusually high or low item rate.
#' @export
parameterBounds<- function() {
  muBound <- 4   #will only consider -4 to +4 for mu
  sigmaBound <- 3 #will only consider 0 to 3 for sigma, otherwise both I (backwards ltrs P2E2)
                  #and Charlie get Ss with sigmas of 4 that are very bad fits, too hard to distinguish apparently
                  #from guessing

  # If fit tries negative sigma values
  # Useful number for when limits can't be exactly zero but can be anything larger
  smallNonZeroNumber <- 10^-3

  #efficacy,          latency,    precision
  lowerBounds <- c(smallNonZeroNumber, -muBound, smallNonZeroNumber)
  upperBounds <- c(1,                   muBound, sigmaBound)

  bounds<- data.frame(lower= lowerBounds, upper= upperBounds)

  row.names(bounds)<- c("efficacy",'latency','precision')

  return( bounds )
}
