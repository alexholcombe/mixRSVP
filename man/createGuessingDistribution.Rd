% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_guessing_distribution.R
\name{createGuessingDistribution}
\alias{createGuessingDistribution}
\title{Create guessing distribution}
\usage{
createGuessingDistribution(minSPE, maxSPE, targetSP, numItemsInStream)
}
\arguments{
\item{minSPE}{Minimum possible serial position error.  ‘minSPE’ ‘maxSPE’ ‘targetSP’ ‘numItemsInStream’}

\item{maxSPE}{Maximum possible serial position error.}

\item{targetSP}{Serial position of target on each trial (array), where first item in the stream is 1, not 0.}

\item{numItemsInStream}{Number of items in each stream.}
}
\value{
The guessing distribution, typically pseudo-uniform (see Goodbourn & Holcombe, 2015).
}
\description{
Create guessing distribution
}
\examples{
createGuessingDistribution(-12,12,c(10,10,11,11,12,12,13,13),22)

}
