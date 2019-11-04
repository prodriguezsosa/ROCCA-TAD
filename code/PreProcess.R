# clean up environment
rm(list = ls())

# libraries

library(pbapply)
library(openxlsx)
library(dplyr)

# set working directory
setwd("~/Dropbox/Research/Tennessee/data")

#---------------
# load data
#---------------

# function 
process_sheet <- function(sheet_name){
  sheet_data <- openxlsx::read.xlsx("Corpus of Articles.xlsx", sheet = sheet_name, detectDates = TRUE)
  if(ncol(sheet_data) == 5){sheet_data$notes <- NA}  # some sheets have an additional column labeled notes
  colnames(sheet_data) <- c("date", "oped", "title", "text", "url", "notes")  # rename columns
  sheet_data$source <- sheet_name  # add the name of the source (extracted from the sheet name)
  return(sheet_data)
}

sheet_names <- getSheetNames("Corpus of Articles.xlsx")  # extract sheet names
processed_data <- pblapply(sheet_names, function(x) process_sheet(x))  # apply pre-processing function to each sheet
processed_data <- do.call(rbind, processed_data)  # bind all sheets together

# mark spanish newspapers
spanish_sources <- c("La Campana del Sur", "La Prensa Latina")
processed_data <- processed_data %>% mutate(spanish = if_else(source %in% spanish_sources, 1, 0))

