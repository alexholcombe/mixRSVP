#Loads raw data from MATLAB files for second backwards paper first experiment
require(R.matlab)

GoogleDrivePath<-"Google\ Drive/Backwards\ paper"

#First experiment of second backwards-letters paper. random orientation each trial.
rawDataPath<- file.path("~",GoogleDrivePath,
                            "secondPaper/E1/Data/RawData/Data")
#raw data path containing .mat file for each subject
rawDataExcludedByMATLABpath<- file.path(rawDataPath, "Excluded")

pathNeeded<- "data-raw"
source(file.path(pathNeeded,"turnMATintoMeltedDataframe.R"))

readInAllFiles<- function(rawDataPath) {

  files <- dir(path=rawDataPath,pattern='.mat')  #find all data files in this directory
  dfAll<-data.frame()
  for (i in 1:length(files)) { #read in each file
    fileThis<- file.path(rawDataPath,files[i])
    rawDataLoad=tryCatch(
      readMat(fileThis),
      error=function(e) {
        stop( paste0("ERROR reading the file ",fname," :",e) )
      } )
    apparentSubjectName <- strsplit(files[i],split="_")[[1]][1]
    subjectName<- rawDataLoad$participantID[[1]]
    if (apparentSubjectName != subjectName) {
      stop( paste0("WARNING apparentSubjectName",apparentSubjectName," from filename does not match subjectName in data structure",subjectName) )
    }
    rawDataLoad$file <- files[i]

    dfThis<- turnMATintoMeltedDataframe(rawDataLoad)

    tryCatch(
      dfAll<-rbind(dfAll,dfThis), #if fail to bind new with old,
      error=function(e) { #Give feedback about how the error happened
        cat(paste0("Tried to merge but error:",e) )
      } )
  }
  return(dfAll)
}

E<- readInAllFiles(rawDataPath)
#Calculate the serial position error
E$SPE<- E$respSP - E$targetSP

sanityVisualCheck<-FALSE
if (sanityVisualCheck) {
  library(ggplot2)
  g=ggplot(E, aes(x=SPE))
  #plot data
  g<-g+geom_histogram(binwidth=1)
  g
}

#Excluded subfolder contains Ss Chris excluded for low efficacy
excludedSsPath<- file.path(rawDataPath,"Excluded")

excluded<- readInAllFiles(excludedSsPath)
#Calculate the serial position error
excluded$SPE<- excluded$respSP - excluded$targetSP

sanityVisualCheck<-FALSE
if (sanityVisualCheck) {
  library(ggplot2)
  g=ggplot(excluded, aes(x=SPE))
  #plot data
  g<-g+geom_histogram(binwidth=1)
  g
}


#Meaning of condition variable
#1 - upright
#2 - inverted

E$excluded<-FALSE
excluded$excluded<-TRUE

data<-rbind(E,excluded)

#save dataframe into proper library data directory
save(data, file = "data/backwards2_E1.rdata")
