
<!-- README.md is generated from README.Rmd. Please edit that file -->
mixRSVP
=======

mixRSVP mixture models temporal errors in reports of an item from an array or a stream, presently using a mixture model of uniform and Gaussian distributions. [Here](https://alexholcombe.wordpress.com/2015/04/05/reporting-items-from-a-stream-and-mixture-modeling-to-reveal-buffering-and-a-bottleneck/) is an explanation of what the hell that means.

Installation
------------

``` r
install_packages('devtools')  #if you don't have devtools already
library('devtools') 
devtools::install_github('alexholcombe/mixRSVP',build_vignettes=TRUE)
```

Example
-------

Fit some of one of the provided datasets and plot the histogram of serial position errors together with the fitted curve.

``` r
 df <-  subset(P2E2pilot, subject=="CB" & target==1 & condition==1)
 plot_hist_with_fit(df, -11, 11, df$targetSP, 16, TRUE, TRUE, TRUE)
```

Help
----

Type `?mixRSVP` or `?<FUNCTIONNAME>` and look at the vignettes that show examples of what you can do.

``` r
vignette(package='mixRSVP')
```

History
-------

Patrick Goodbourn programmed mixture modeling of RSVP serial position errors in MATLAB. [Certifiedwaif](https://github.com/certifiedwaif/) did initial [port](https://github.com/certifiedwaif/AttentionalBlink) of this to R, largely using automated code translation, and then Alex [functionfied and improved things](https://github.com/alexholcombe/MixtureModelRSVP) before making it into [this package](https://github.com/alexholcombe/mixRSVP).
