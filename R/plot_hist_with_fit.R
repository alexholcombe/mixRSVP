
#' Plot histogram with fitted curve
#'
#' @import ggplot2
#'
#' @param annotateIt Add text to the plot indicating fit efficacy, latency, precision, and negative log likelihood
#' @param showIt Show plot, otherwise only return plot object without actually showing it
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' df <-  subset(P2E2pilot,subject=="CB" & target==1 & condition==1)
#' plot_hist_with_fit(df, -11, 11, df$targetSP, 16, TRUE, TRUE, TRUE)
#'  }
#'
plot_hist_with_fit<- function(df,minSPE,maxSPE,targetSP,numItemsInStream,
                              plotContinuousGaussian,annotateIt,showIt) {
  #targetSP is needed to construct empirical guessing distribution
  assertthat::assert_that(length(data) > 0)

  #calculate curves (predicted heights of bins for each component and combination of components
  curveDfs<- calc_curves_dataframes(df,minSPE,maxSPE,numItemsInStream) #this also does the parameter estimation

  if (plotContinuousGaussian) {
    #Calculate continuous fitted Gaussian, not discrete version.
    grain<-.05
    numObservations<- length(df$SPE)
    gaussianThis<- gaussian_scaled(curveDfs$efficacy[1],curveDfs$latency[1],curveDfs$precision[1],
                                         numObservations,minSPE,maxSPE,grain)
  }

  #plot data
  g= ggplot(df, aes(x=SPE)) + theme_apa()
  #plot data
  g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
  if (plotContinuousGaussian) {
    g<-g + geom_line(data=gaussianThis,aes(x=x,y=gaussianFreq),color="darkblue",size=1.2)
  }
  g<-g+ geom_line(data=curveDfs,aes(x=x,y=guessingFreq),color="yellow",size=1.2)
  g<-g+ geom_line(data=curveDfs,aes(x=x,y=gaussianFreq),color="lightblue",size=1.2)
  g<-g+ geom_point(data=curveDfs,aes(x=x,y=combinedFitFreq),color="green",size=1.2)

  if (annotateIt) {
    g<-g + geom_text(data=curveDfs,aes(x=-9,y=32, label = paste("-logLik==", round(val,1), sep = "")), parse=TRUE,hjust="left") +
      geom_text(data=curveDfs,aes(x=-7,y=28, label = paste("plain(e)==", round(efficacy,2), sep = "")),  parse=TRUE,hjust="left") +
      geom_text(data=curveDfs,aes(x=-7,y=25, label = paste("mu==", round(latency,2), sep = "")),  parse=TRUE,hjust="left")+
      geom_text(data=curveDfs,aes(x=-7,y=22, label = paste("sigma==", round(precision,2), sep = "")), parse=TRUE,hjust="left")
  }
  if (missing(showIt)) {
    showIt = TRUE
  }
  if (showIt) {
    show(g)
  }
  return(g)
}



#' APA compatible ggplot2 theme
#'
#' A ggplot2 theme with a white panel background, no grid lines, large axis and legend titles,
#' and increased text padding for better readability.
#'
#' @details This theme is an adaptation of \code{\link[ggplot2]{theme_bw}}. In ggplot2, themes set the
#'    general aspect of the plot such as the colour of the background, gridlines, the size and colour
#'    of fonts.
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'    # Copied from ?ggtheme
#'    p <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg,
#'      colour = factor(gear))) + facet_wrap(~ am)
#'    p
#'    p + theme_apa()
#'  }

theme_apa <- function(base_size = 12, base_family = "", box = FALSE) {
  adapted_theme <- ggplot2::theme_bw(base_size, base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.1), margin = ggplot2::margin(0, 0, ggplot2::rel(14), 0))

      # , axis.title = ggplot2::element_text(size = ggplot2::rel(1.1))
      , axis.title.x = ggplot2::element_text(size = ggplot2::rel(1), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(ggplot2::rel(12), 0, 0, 0))
      , axis.title.y = ggplot2::element_text(size = ggplot2::rel(1), lineheight = ggplot2::rel(1.1), margin = ggplot2::margin(0, ggplot2::rel(12), 0, 0))
      , axis.ticks.length = ggplot2::unit(ggplot2::rel(6), "points")
      , axis.text = ggplot2::element_text(size = ggplot2::rel(0.9))
      , axis.text.x = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(ggplot2::rel(6), 0, 0, 0))
      , axis.text.y = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin(0, ggplot2::rel(6), 0, 0))
      , axis.line.x = ggplot2::element_line()
      , axis.line.y = ggplot2::element_line()

      , legend.title = ggplot2::element_text()
      , legend.key = ggplot2::element_rect(fill = NA, color = NA)
      , legend.key.width = ggplot2::unit(ggplot2::rel(20), "points")
      , legend.key.height = ggplot2::unit(ggplot2::rel(20), "points")
      , legend.margin = ggplot2::margin(
        t = ggplot2::rel(16)
        , r = ggplot2::rel(16)
        , b = ggplot2::rel(16)
        , l = ggplot2::rel(16)
        , unit = "points"
      )

      , panel.spacing = ggplot2::unit(ggplot2::rel(14), "points")
      , panel.grid.major.x = ggplot2::element_line(size = NA)
      , panel.grid.minor.x = ggplot2::element_line(size = NA)
      , panel.grid.major.y = ggplot2::element_line(size = NA)
      , panel.grid.minor.y = ggplot2::element_line(size = NA)

      , strip.background = ggplot2::element_rect(fill = NA, color = NA)
      , strip.text.x = ggplot2::element_text(margin = ggplot2::margin(0, 0, ggplot2::rel(16), 0)) # size = ggplot2::rel(1.1),
      , strip.text.y = ggplot2::element_text(margin = ggplot2::margin(0, 0, 0, ggplot2::rel(16))) # size = ggplot2::rel(1.1),
    )

  if(box) {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_rect(color = "black"))
  } else {
    adapted_theme <- adapted_theme + ggplot2::theme(panel.border = ggplot2::element_blank())
  }

  adapted_theme
}


