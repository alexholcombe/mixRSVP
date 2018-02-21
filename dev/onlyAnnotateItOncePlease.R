
data <- backwards2_E1 #A dataset provided as an example with the package
#.mat file been preprocessed into melted long dataframe
numItemsInStream <- length( data$letterSeq[1,] )
data$letterSeq <- NULL
library(dplyr)

#To use dplyr operations, each column must be a 1d atomic vector or a list. So, can't have array fields like letterSeq
data$letterSeq<- NULL

#Give conditions better names than 1 and 2
names(data)[names(data) == 'target'] <- 'stream'
data <- data %>% mutate( stream =ifelse(stream==1, "Left","Right") )
#mutate condition to Orientation
names(data)[names(data) == 'condition'] <- 'orientation'
data <- data %>% mutate( orientation =ifelse(orientation==1, "Canonical","Inverted") )


condtnVariableNames <- c("subject","orientation", "stream") #

#there are twenty-some subjects, but analysing all would make the vignette far too long to build
df<- data %>% dplyr::filter(subject>="AE",subject<="AG") #Includes one or two who fail the likelihood ratio test, for illustration

estimates<- df %>%
  group_by_(.dots = condtnVariableNames) %>%  #.dots needed when you have a variable containing multiple factor names
  do(  analyzeOneConditionDF(.,numItemsInStream,parameterBounds(), nReplicates=3)  )

#Add R parameter estimates to dataframe. That way calc_curves_dataframe won't have to refit the data.
dg<- merge(df,estimates)

curves<- dg %>% group_by_at(.vars = condtnVariableNames) %>%
  do(calc_curves_dataframe(.,minSPE,maxSPE,numItemsInStream))


KW <- df %>%
  group_by(Species, treatment) %>%
  summarise(p=round(kruskal.test(value ~ both)$p.value,2),
            y=min(value),
            x=1) %>%
  ungroup() %>%
  mutate(y=min(y))


g<- plot_hist_with_fit(df,minSPE,maxSPE,df$targetSP,numItemsInStream,plotContinuousGaussian,annotateIt, FALSE)
#show(g)
