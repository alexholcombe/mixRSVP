* Did use_r("createGuessingDistribution")
* Did use_roxygen_md()
* Did use_data_raw() - this will not be visible to package users I think. Add data creation scripts there, then use_data to create proper package data format.
* Did use_data(P2E2pilot)
* Did use_package('optimx').  Refer to functions with `optimx::fun()`

# The first practical advantage to using a package is that it’s easy to re-load your code. # You can either run devtools::load_all()

* Did use_package('stats') because even though stats automatic, required by CRAN to import non-base stuff and use_package('stats') added it to imports I think. But still build complains that dnorm etc still not defined as global variables and I should "Consider adding importFrom("stats", "dnorm", "pnorm", "runif") to NAMESPACE file"
devtools::document()
Fixed this by adding the following to the R files so roxygen will do the right thing
#' @importFrom stats dnorm pnorm runif
#'
NULL

* Did use_testthat()

> use_vignette('hist_with_fit')
✔ Adding 'knitr' to Suggests field in DESCRIPTION
✔ Setting VignetteBuilder field in DESCRIPTION to 'knitr'
✔ Adding 'rmarkdown' to Suggests field in DESCRIPTION
✔ Creating 'vignettes/'
✔ Adding '*.html', '*.R' to 'vignettes/.gitignore'
✔ Adding 'inst/doc' to './.gitignore'
✔ Creating 'vignettes/hist_with_fit.Rmd'
● Modify 'vignettes/hist_with_fit.Rmd'