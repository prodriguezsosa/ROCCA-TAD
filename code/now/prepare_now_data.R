rm(list = ls())

# --------------------------------
# setup
# --------------------------------

# libraries
library(readr)
library(stringr)
library(lubridate)
library(dplyr)
library(pbapply)
library(progress)
library(foreach)
library(doParallel)

# paths
in_path_texts <- "/Volumes/Potosi/Research/Data/NOW/Text/"
in_path_sources <- "/Volumes/Potosi/Research/Data/NOW/Sources/"
out_path_data <- "~/Dropbox/GitHub/large_data/ROCCA-TAD/now/"

# --------------------------------
# load data
# --------------------------------
text_files <- as.list(list.files(in_path_texts))
#date <- str_extract_all(files, pattern = "\\d+-\\d+") %>% unlist %>% paste(., "01", sep = "-") %>% ymd

# pre-process function  (SUB-FUNCTION)
pre_process <- function(text){
  text_i <- gsub("[^[:alpha:]]", " ", text) # remove all non-alpha characters
  text_i <- str_replace_all(text_i, "\\b\\w{1,2}\\b", "") # remove 1-2 letter words
  text_i <- str_replace_all(text_i, "^ +| +$|( ) +", "\\1")  # remove excess white space
  #text_i <- tolower(text_i)  # lowercase
  return(text_i)
}

# pre-process function (SUB-FUNCTION)
load_and_preprocess <- function(text_file_i, us_file_i, tokens_of_interest){
  text <- read.delim(unz(paste0(in_path_texts, text_file_i), us_file_i), header = FALSE, quote = "", stringsAsFactors = FALSE) %>% rename("text" = "V1")
  text <- text[grepl("^@@\\d+", text$text),]
  text_tibble <- tibble(text_id = unlist(str_extract_all(text, pattern = "^@@\\d+")), text = str_remove(text, pattern = "^@@\\d+"))
  text_tibble <- text_tibble %>% mutate(text_id = str_remove(text_id, "@@"))
  text_tibble <- text_tibble %>% mutate(text = lapply(text, pre_process) %>% unlist)  # apply pre-processing function
  text_tibble <- text_tibble %>% mutate(date = str_extract_all(text_file_i, pattern = "\\d+-\\d+") %>% unlist %>% paste(., "01", sep = "-") %>% ymd)
  text_tibble <- text_tibble[text_tibble$text!="",] # remove nuls
  if(!is.null(tokens_of_interest)) text_tibble <- text_tibble[grepl(paste(tokens_of_interest, collapse = "|"), text_tibble$text),]
  return(text_tibble)
}

# load, pre_process & subset text
load_text <- function(text_file_i, tokens_of_interest = NULL){
  #text_file_i <- text_files[[i]]
  df_of_files <- unzip(paste0(in_path_texts, text_file_i), list = TRUE) 
  us_files <- df_of_files$Name[grepl(paste(c("us", "US"), collapse = "|"), df_of_files$Name)]
  text_tibble <- lapply(us_files, function(x) load_and_preprocess(text_file_i, x, tokens_of_interest)) %>% do.call(rbind, .)
  return(text_tibble)
}

# parallelize
library(foreach)
library(doParallel)
#library(doSNOW)
numCores <- detectCores()
cl <- makeCluster((numCores - 8), outfile="") # number of cores. Notice 'outfile'
registerDoParallel(numCores)
clusterEvalQ(cl, .libPaths("/Users/pedrorodriguez/Dropbox/GitHub/environments/NeuroPolitics/embeddings/packrat/lib/x86_64-apple-darwin15.6.0/3.6.2"))
#clusterEvalQ(cl, library(doParallel))
#registerDoSNOW(cl)
iterations <- length(text_files)
pb <- txtProgressBar(min = 1, max = iterations, style = 3)
result_texts <- foreach(i = 1:iterations, .combine = rbind, .packages=c("dplyr", "stringr", "lubridate")) %dopar% {
    s <- load_text(text_files[[i]], tokens_of_interest = NULL)
    setTxtProgressBar(pb, i) 
    return(s)
  }
close(pb)
stopCluster(cl)

saveRDS(result_texts, paste0(out_path_data, "results_texts.rds"))

# media of interest
#"The Sean Hannity Show", 
sources_of_interest <- c("cnn",  "washington post", "npr", "pbs", "new yorker", "the atlantic", "new york times", 
                         "wall street journal", "cbs local", "cbs news", "fox news", "foxnews",  "foxsports.com", 
                         "nfl.com", "breitbart news", "abc news", "cbs news", "msnbc", "msnbc.com", "nbcnews.com", 
                         "usa today", "los angeles times", "vice", "vice news", "huffington post", "tmz.com", "newsweek", 
                         "time magazine",  "time", "the guardian", "univision", "rushlimbaugh.com", "bbc news", 
                         "the guardian", "buzzfeed", "new york post", "newsweek", "politico", "vox.com", 
                         "business insider", "the hill", "daily caller", "washington examiner", "cnbc.com", "cnbc", 
                         "cnbc (subscription)")

# load sources
load_and_process_sources <- function(source_file_i, countries_of_interest = NULL, sources_of_interest = NULL){
  df_of_source_files <- unzip(paste0(in_path_sources, source_file_i), list = TRUE) 
  source_tibble <- read.delim(unz(paste0(in_path_sources, source_file_i), df_of_source_files$Name), header = FALSE, quote = "", stringsAsFactors = FALSE, skipNul = TRUE) %>% 
    rename("text_id" = "V1", "num_tokens" = "V2", "full_date" = "V3", "country" = "V4", "source" = "V5", "url" = "V6", "first_sentence" = "V7")
  source_tibble <- source_tibble %>% mutate(text_id = as.character(text_id), full_date = ymd(full_date), source = str_trim(tolower(iconv(source))))
  if(!is.null(countries_of_interest)) source_tibble <- source_tibble %>% filter(country %in% countries_of_interest)
  if(!is.null(sources_of_interest)) source_tibble <- source_tibble[grepl(paste(sources_of_interest, collapse = "|"), source_tibble$source),]
  return(source_tibble)
}

#https://www.journalism.org/2014/10/21/political-polarization-media-habits/pj_14-10-21_mediapolarization-08/

source_files <- as.list(list.files(in_path_sources))
#test_source <- load_and_process_sources(source_files[2], countries_of_interest = c("us", "US"), sources_of_interest = NULL)
countries_of_interest = c("us", "US")
cl <- makeCluster((numCores - 8), outfile="") # number of cores. Notice 'outfile'
registerDoParallel(numCores)
clusterEvalQ(cl, .libPaths("/Users/pedrorodriguez/Dropbox/GitHub/environments/NeuroPolitics/embeddings/packrat/lib/x86_64-apple-darwin15.6.0/3.6.2"))
iterations <- length(source_files)
pb <- txtProgressBar(min = 1, max = iterations, style = 3)
result_sources <- foreach(i = 1:iterations, .combine = rbind, .packages=c("dplyr", "stringr", "lubridate")) %dopar% {
  s <- load_and_process_sources(source_files[i], countries_of_interest = NULL, sources_of_interest = sources_of_interest)
  setTxtProgressBar(pb, i) 
  return(s)
}
close(pb)
stopCluster(cl)

# join results
results_join <- left_join(result_texts, result_sources, by = "text_id")
saveRDS(results_join, paste0(out_path_data, "news_corpus.rds"))