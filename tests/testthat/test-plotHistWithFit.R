# test of histogramPlotting intended to be used with the testthat package

#To testthat, run test_file("mixtureModeling/tests/test_histogramPlotting.r")



data<- readRDS( file.path(pathNeeded,"tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
#data<- readRDS( file.path("mixtureModeling/tests", "alexImportBackwardsPaper2E1.Rdata") ) #.mat file been preprocessed into melted long dataframe
library(dplyr)
numItemsInStream<- length( data$letterSeq[1,] )
#It seems that to work with dplyr, can't have array field like letterSeq
data$letterSeq<- NULL


#plot histogram
require(ggplot2)
minSPE<- -17; maxSPE<- 17

#Give conditions better names than 1 and 2
names(data)[names(data) == 'target'] <- 'stream'
data <- data %>% mutate( stream =ifelse(stream==1, "Left","Right") )
#mutate condition to Orientation
names(data)[names(data) == 'condition'] <- 'orientation'
data <- data %>% mutate( orientation =ifelse(orientation==1, "Canonical","Inverted") )

#Test on one subject
df<-data %>% dplyr::filter(subject=="BE")

#Accomplish same as plotHistWithFit functionality
curveDfs<- calcCurvesDataframes(df,minSPE,maxSPE,numItemsInStream)

g=ggplot(df, aes(x=SPE)) + theme_apa()
#plot data
g<-g+geom_histogram(binwidth=1) + xlim(minSPE,maxSPE)
g<-g+ geom_line(data=curveDfs,aes(x=x,y=guessingFreq),color="yellow",size=1.2)
g<-g+ geom_line(data=curveDfs,aes(x=x,y=gaussianFreq),color="lightblue",size=1.2)
g<-g+ geom_point(data=curveDfs,aes(x=x,y=combinedFitFreq),color="green",size=1.2)


#Also plot finer-grained theoretical Gaussian
#   estimates<- analyzeOneCondition(df,numItemsInStream,parameterBounds())
efficacy<- curveDfs$efficacy[1]; latency<- curveDfs$latency[1]; precision<- curveDfs$precision[1]
grain<-.05
numObservations<- length(df$SPE)
#Scale up the numObservations to compensate for the smaller grain size
numObservations<- numObservations
gaussianThis<- gaussianScaledForData(efficacy,latency,precision,numObservations,minSPE,maxSPE,grain)
g<-g + geom_line(data=gaussianThis,aes(x=x,y=gaussianFreq),color="darkblue",size=1.2)


######

df<-data %>% dplyr::filter(subject=="BE")
plotContinuousGaussian<-TRUE
g<- plotHistWithFit(df,minSPE,maxSPE,df$targetSP,numItemsInStream,plotContinuousGaussian)
show(g)

#Check troublesome cases.

# BE,2,1
df<- data %>% dplyr::filter(subject=="BE" & stream=="Right" & orientation=="Canonical")
#estimates<- analyzeOneCondition(df,numItemsInStream,parameterBounds())
g<- plotHistWithFit(df,minSPE,maxSPE,df$targetSP,numItemsInStream,plotContinuousGaussian)
show(g) #Looks like efficacy wrong. Problem is that can fit the histogram very well with a tiny precision.
#This looks like a general problem that will often cause precision to be underestimated.

#BA right Inverted
df <- data %>% dplyr::filter(subject=="BA" & stream=="Right" & orientation=="Inverted")
#estimates<- analyzeOneCondition(df,numItemsInStream,parameterBounds())
g<- plotHistWithFit(df,minSPE,maxSPE,df$targetSP,numItemsInStream,plotContinuousGaussian)
show(g) #Looks fine

#BO,1,1
df<- data %>% dplyr::filter(subject=="BO" & stream=="Left" & orientation=="Canonical")
#estimates<- analyzeOneCondition(df,numItemsInStream,parameterBounds())
g<- plotHistWithFit(df,minSPE,maxSPE,df$targetSP,numItemsInStream,plotContinuousGaussian)
show(g) #How did BO do it? Never guessed, almost, only twice

#Get Error in eigen(nhatend) : infinite or missing values in 'x'
# which optimx's author says might be fine:
#http://r.789695.n4.nabble.com/Error-in-eigen-nhatend-td4708274.html



#estimates %>% dplyr::filter(round(p1,3)==0.280) #BA,2,2 or BE,2,1
#1e-05  4 1e-05 334.385593546 but didn't end up being the winning replicate
#Inspect AD,2,1 and AI,1,2 because very poor fit

