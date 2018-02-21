#' Format a p-value appropriately.
#' with < precision if small enough, otherwise = p
#'
#' @param p The p-value to format
#' @return a string
#' @export
#'
#'
format_p <- function(p, precision=0.001) {
  #It seems format.p needs to be exported (in roxygen comment above) because it seems to be "lost" otherwise when leavel local environment and ggplot object re-built
  #This is much like format.pval but avoids scientific notation and adds an equal if no less-than applies
  digits <- -log(precision, base=10)
  ptext <- formatC(p, format='f', digits=digits)

  ptext[ptext == formatC(0, format='f', digits=digits)] <- paste0('<', precision)
  #if "<" has not been put in the string, insert "="
  if (startsWith(ptext,"0")[1]) { #because "<" has not been prepended, is something like "0.303"
    #Warning tells me that sometimes result of startsWith has length greater than 1 but I don't know why.
    #It gets sent 35 copies just for one hist. I think it's writing one copy of the text for each data point
    ptext <- paste0("=",ptext)  # ptext[1] <- "="  #This won't work because only sets the first one, but there are 35
    #print("inserted =")
  }
  #ppp<-ptext
  #print(paste0("ptext at end=",ptext))
  ptext
}

#' Annotate histogram with parameter vals and stats
#' Returns the ggplot object you passed it, but annotated.
#' @import ggplot2 dplyr methods
#'
#' @param g The ggplot object to annotate
#' @param curvesDf A dataframe containing columns for the parameter values for each condition
#'
#' @export
#'
#'
annotate_fit <- function(g,curvesDf) {

  #It seems that for this function to find format.p when called from the top-level environment, format.p has to be part of it.
  #Yet when it is called by plot_hist_with_fit, it doesn't! Somehow I fixed this by changing name to format_p


  #mixSig - whether mixture model statistically significantly better than guessing
  #to avoid writing the text one time for each data point, cut to one trial per condition
  #textDf<- curvesDf[1,] #only need one x-value (SPE), each one has the same efficacy latency etc. But then only works for first plot
  #Need to keep one x from each condition, but don't know variables whose combinations define the conditions
  textDf<- curvesDf
  textDf <- dplyr::mutate(textDf, mixSig = ifelse(pLRtest <= .05, TRUE, FALSE))


  x <- layer_scales(g)$x$range$range[1] + 3 #xlim minimum
  yLimMax<- layer_scales(g)$y$range$range[2]
  y<-yLimMax*.95
  ySpaceToOccupy <- y/2
  ys<- seq(y,y-ySpaceToOccupy,length.out=5)
  g<-g+
    geom_text(data=textDf, x=x, y= ys[1], aes(label = paste("plain(e)==", round(efficacy,2), sep = "")),  parse=TRUE,hjust="left")+
    geom_text(data=textDf, x=x, y= ys[2], aes(label = paste("mu==", round(latency,2), sep = "")),  parse=TRUE,hjust="left")+
    geom_text(data=textDf, x=x, y= ys[3], aes(label = paste("sigma==", round(precision,2), sep = "")), parse=TRUE,hjust="left")
    if ( "pLRtest" %in% names(textDf) ) {
      g<-g + geom_text(data=textDf,x=x-1,y=ys[4],aes(label = paste("-logLik==", round(val,1), sep = "")), parse=TRUE,hjust="left")
      #add color for p-value
      if ( "mixSig" %in% names(textDf)) {
        g<- g + geom_text(data=textDf,x=x, y=ys[5],
                          aes(label = paste0("p",mixRSVP::format_p(pLRtest)), color=mixSig), parse=FALSE,hjust="left")
        #g<- g + geom_text(data=curvesDf,x=x, y=ys[5],
        #                  aes(label = paste0("p=",format.pval(pLRtest,3,eps=.001)), color=mixSig), parse=FALSE,hjust="left")
        #colorMapping <- c("FALSE" = "red", "TRUE" = "forestgreen")
        #g<- g + scale_color_manual(values = colorMapping) + guides(color=FALSE) #set colors and remove legend`

        g<- g + scale_color_manual(values=c("forestgreen","red")) + guides(color=FALSE) #set colors and remove legend`
      }
  }
  return (g)
}

#' Plot histogram with fitted curve
#'
#' @import ggplot2
#'
#' @param df A dataframe that must have fields targetSP and SPE
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
  assertthat::assert_that(length(df) > 0)

  #calculate curves (predicted heights of bins for each component and combination of components
  curvesDf<- calc_curves_dataframe(df,minSPE,maxSPE,numItemsInStream) #this also does the parameter estimation

  if (plotContinuousGaussian) {
    #Calculate continuous fitted Gaussian, not discrete version.
    grain<-.05
    numObservations<- length(df$SPE)
    gaussianThis<- gaussian_scaled(curvesDf$efficacy[1],curvesDf$latency[1],curvesDf$precision[1],
                                         numObservations,minSPE,maxSPE,grain)
  }

  #plot data
  g= ggplot(df, aes(x=SPE)) + theme_apa()
  #plot data
  g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
  if (plotContinuousGaussian) {
    g<-g + geom_line(data=gaussianThis,aes(x=x,y=gaussianFreq),color="darkblue",size=1.2)
  }
  g<-g+ geom_line(data=curvesDf,aes(x=x,y=guessingFreq),color="yellow",size=1.2)
  g<-g+ geom_line(data=curvesDf,aes(x=x,y=gaussianFreq),color="lightblue",size=1.2)
  g<-g+ geom_point(data=curvesDf,aes(x=x,y=combinedFitFreq),color="green",size=1.2)

  if (annotateIt) {

    g<- annotate_fit(g,curvesDf) #assumes curvesDf includes efficacy,latency,precision
  }
  if (missing(showIt)) {
    showIt = TRUE
  }
  if (showIt) {
    methods::show(g)
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


