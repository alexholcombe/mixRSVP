#To testthat, run test_file("mixtureModeling/tests/test_calcCurvesDataframes.R")
#Compensate for path getting set to mixtureModeling/tests/
if (basename(getwd()) != "tests") {
  pathNeeded<- "mixtureModeling"
} else { 
  pathNeeded <- ".." 
}

source( file.path(pathNeeded,"calcCurvesDataframes.R") ) #for calcFitDataframes

#Read in some data to fit
df<-readRDS( file.path(pathNeeded,"tests","exampleSubject.Rdata") )
library(dplyr)
df<- dplyr::filter(df, condition==1 & target==1)
numItemsInStream<-24
minSPE<- -17; maxSPE<- 17
curveDfs<- calcCurvesDataframes(df,minSPE,maxSPE,numItemsInStream)
