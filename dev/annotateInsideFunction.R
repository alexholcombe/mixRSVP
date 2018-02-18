library(ggplot2)

textDf<- data.frame(x=3,mytext="HELLO WORLD")

g<-ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()


annotate_it <- function(g, textDf) {
  yLimMax<- layer_scales(g)$y$range$range[2]
  yAnnotate<- yLimMax / 2
  g<-g+  geom_text(data=textDf,aes(x=x, label = mytext), y=yAnnotate )
  return (g)
}

g<- annotate_it(g,textDf)
g

#####################


g<-ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()

textDf<- data.frame(x=3,mytext="HELLO WORLD")

  yLimMax<- layer_scales(g)$y$range$range[2]
  yAnnotate<- yLimMax / 2
  g<-g+  geom_text(data=textDf,aes(x=x,y=yAnnotate, label = mytext) )

g

#####
#See https://stackoverflow.com/questions/48820489/force-evaluation-to-static-value-of-variable-passed-to-aes for a cool solution


### Programmatically set text color
