rm(list = ls())  # wipe clean

packrat::init("/Users/angieliang/Desktop/ROCCA/Refugees/TAD")
packrat::on()

#load libraries
library(pbapply)
library(openxlsx)
library(dplyr)

#set wd
setwd("~/Dropbox/GitHub/repositories/ROCCA-TAD/data/")
getwd()

## PREPROCESSING
#load the data
process_sheet <- function(sheet_name){
  sheet_data <- openxlsx::read.xlsx("Corpus_Local_Memphis.xlsx", sheet = sheet_name, detectDates = TRUE)
  if(ncol(sheet_data) == 5){sheet_data$notes <- NA}  # some sheets have an additional column labeled notes
  colnames(sheet_data) <- c("date", "oped", "headline", "text", "url", "notes")  # rename columns
  sheet_data$source <- sheet_name  # add the name of the source (extracted from the sheet name)
  return(sheet_data)
}

sheet_names <- getSheetNames("Corpus_Local_Memphis.xlsx")  # extract sheet names
processed_data <- pblapply(sheet_names, function(x) process_sheet(x))  # apply pre-processing function to each sheet
processed_data <- do.call(rbind, processed_data)  # bind all sheets together

# generate a document ID
processed_data <- processed_data %>% mutate(document = 1:nrow(processed_data))

# save processed data (to not have to repeat code above)
saveRDS(processed_data, "corpus_local_Memphis.rds")

## End preprocessing

#Read in RDS
local_media_corpus <- readRDS("corpus_local_Memphis.rds")

glimpse(local_media_corpus)



