fileWithPath<- file.path("data-raw","P2E2_PilotData.Rdata")

if (file.exists(fileWithPath)) {
  P2E2pilot<- readRDS( fileWithPath ) #.mat file been preprocessed into melted long dataframe
} else {
  print("Error! Could not find file. Probably your working directory is bad")
}
