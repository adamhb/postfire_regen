library(tidyverse)


getCSVfromTranscript <- function(file = '~/cloud/gdrive/fire_project/local_data/field_validation/B64_transcript.txt',
                                 plotID, makeCSV = F){
  transcript <- read_file(file)
  transcript <- gsub(pattern = c("conifer|say|charlie|Charlie|callfor|Drive|Charly|Charlene|charlene") ,replacement = "C",x = transcript)
  transcript <- gsub(pattern = c("shrub|sierra|Sierra|Ciara|ciara|shower|yes") ,replacement = "S", x = transcript)
  transcript <- gsub(pattern = c("dad|dead|Dead|delta|Delta") ,replacement = "D", x = transcript)
  transcript <- gsub(pattern = c("broadleaf|bravo|Bravo|Bradley") ,replacement = "R", x = transcript)
  transcript <- gsub(pattern = c("bear|bare|Bear") ,replacement = "B", x = transcript)
  transcript <- gsub(pattern = c("grass|Grass") ,replacement = "B", x = transcript)
  transcript <- gsub(pattern = " ",replacement = "",x = transcript)
  transcript <- gsub(pattern = "\n",replacement = "",x = transcript)
  transcript <- gsub(pattern = "[[:digit:]]",replacement = "",x = transcript)
  
  if(makeCSV == T){
    write_csv(x = as.tibble(strsplit(transcript,split = "")[[1]]),file = paste0('~/cloud/gdrive/fire_project/local_data/field_validation/',plotID,"clean_transcript.csv"))
  }else{
    return(transcript)
  }
 
}

getCSVfromTranscript(file = '~/cloud/gdrive/fire_project/local_data/field_validation/B64_transcript.txt', plotID = "B64",makeCSV = T)
  

  
