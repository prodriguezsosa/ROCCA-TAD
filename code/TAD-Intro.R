rm(list = ls())  # wipe clean

# TIPS:
# you should always (always!) annotate your code
# use version control (GitHub)
# DEBUGGING: rubber duck it
# Google is your friend. Type your question and add "R" to the end of it.
# for bigger projects: use a dependency manager (packrat)
# key packages: tidytext, quanteda, stringr

# --------------------------------
# initialize environment
# --------------------------------
packrat::init("~/Dropbox/GitHub/environments/ROCCA-TAD/")
packrat::on()

#-----------------------------
# 1 SETTING UP
#-----------------------------

# load libraries
library(dplyr)  # for data manipulation

# set working directory (where our data is stored)
setwd("~/Dropbox/GitHub/repositories/ROCCA-TAD/data/")  # set working directory
getwd()  # returns current working directory

# load data
local_media_corpus <- readRDS("now_local_media_corpus.rds")

#-----------------------------
# QUICK VIEW OF THE DATA
#-----------------------------

# glimpse
glimpse(local_media_corpus)
dim(local_media_corpus)
nrow(local_media_corpus)
ncol(local_media_corpus)

# using the dollar sign to select specific variables
local_media_corpus$first_sentence[1:5]
head(local_media_corpus$first_sentence)
tail(local_media_corpus$first_sentence)

# using matrix notation
local_media_corpus[1:5, "first_sentence"]
local_media_corpus[1:5, c("first_sentence", "num_tokens")]

# using dplyr
local_media_corpus %>% select(first_sentence, num_tokens) %>% slice(1:5)
local_media_corpus %>% filter(source == "nashville scene") %>% select(first_sentence) %>% head()

# table
table(local_media_corpus$source)

# which newspapers have the most articles?
local_media_corpus %>% group_by(source) %>% summarise(count = n()) %>% arrange(-count) %>% head  # pure count
local_media_corpus %>% group_by(source) %>% summarise(count = n()) %>% mutate(freq = count/sum(count)) %>%  arrange(-freq) %>% head  # frequency

#-----------------------------
# CREATING VARIABLES
#-----------------------------
library(lubridate)  # for dates

# dollar sign
local_media_corpus$year <- year(local_media_corpus$date)

# matrix identifier
local_media_corpus[, "year1"]  <- year(local_media_corpus[, "date"])

# dplyr
local_media_corpus <- local_media_corpus %>% mutate(year2 = year(date))

# should be the same
all.equal(local_media_corpus$year, local_media_corpus$year1)
all.equal(local_media_corpus$year1, local_media_corpus$year2)

# drop variable
local_media_corpus$year1 <- NULL
"year1" %in% colnames(local_media_corpus)  # one way to check if deleted column was actually deleted
local_media_corpus[, "year2"] <- NULL  # using matrix notation
local_media_corpus <- local_media_corpus %>% select(-year)  # using dplyr

# let's check the distribution of articles over the years
local_media_corpus <- local_media_corpus %>% mutate(year = year(date))
table(local_media_corpus$year)

#-----------------------------
# VISUALIZATION
#-----------------------------
# base R
barplot(table(local_media_corpus$year), 
        main="Number of Articles", 
        xlab="Year")

# ggplot2
library(ggplot2)  # for plotting
local_media_corpus %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  arrange(year) %>% 
  na.omit() %>%  # two articles have no date assigned to them
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity") + 
  xlab("Year") + ylab("Number of Articles'") +
  theme_minimal()

#-----------------------------
# LOOPS, FUNCTIONS, APPLY
#-----------------------------
# functions
library(stringr)  # for manipulating strings (i.g. character variables)

count_tokens <- function(text){
  return(str_count(text, " ") + 1)
}

# loop
doc_length <- vector()
for(i in 1:nrow(local_media_corpus)){
  doc_length[i] <- count_tokens(local_media_corpus$first_sentence[i])
}

# vectorized
doc_length2 <- count_tokens(local_media_corpus$first_sentence)

# use apply functions
doc_length3 <- lapply(local_media_corpus$first_sentence, count_tokens)

#-----------------------------
# PRE-PROCESSING
#-----------------------------
local_media_corpus$text <- gsub("[^[:alpha:]]", " ", local_media_corpus$text)  # remove all non-alpha characters
local_media_corpus$text <- str_replace_all(local_media_corpus$text, "\\b\\w{1,2}\\b", "")  # remove 1-2 letter words
local_media_corpus$text <- tolower(local_media_corpus$text)  # lower case
local_media_corpus$text <- str_replace_all(local_media_corpus$text, "^ +| +$|( ) +", "\\1")  # strip whitespace

# do the same but using a function
pre_process_fcn <- function(text){
  text <- gsub("[^[:alpha:]]", " ", text)  # remove all non-alpha characters
  text <- str_replace_all(text, "\\b\\w{1,2}\\b", "")  # remove 1-2 letter words
  text <- tolower(text)  # lower case
  text <- str_replace_all(text, "^ +| +$|( ) +", "\\1")  # strip whitespace
  return(text)
}

local_media_corpus$first_sentence <- pre_process_fcn(local_media_corpus$first_sentence)

#-----------------------------
# REGULAR EXPRESSIONS
#-----------------------------
# regular expressions are a very powerful tool in wrangling text
# cheatsheet for regex: https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
# see other non-base packages e.g. stringr

# grep
refugee_index <- grep("refugee", local_media_corpus$first_sentence)
head(local_media_corpus$first_sentence[refugee_index])

# grepl
refugee_index <- grepl("refugee", local_media_corpus$first_sentence)
table(refugee_index)

# this returns every speech that starts with the word Immigrant
grep("^refugee", local_media_corpus$first_sentence, value = TRUE)
grepl("^so", c("so today we", "never so today", "today never so"))  # test in smaller cases

# substitute words
gsub("Immigrant", "immigrant",  "Immigrant group blasts Tennessee bill to limit prenatal care")

#-----------------------------
# KEY WORDS IN CONTEXT
#-----------------------------
library(quanteda)  # for text analysis
library(DT)  # for table handling
# other "similar" packages worth looking at: tm, tidytext

# explore uses of a given word (kwic = (k)ey (w)ord (i)n (c)ontext)
kwic_refugee <- kwic(local_media_corpus$text, pattern = "refugee", window = 10)

#-----------------------------
# WORDCLOUD
#-----------------------------
# quanteda requires we create a document feature matrix (will discuss next time)
refugee_dfm1 <- dfm(local_media_corpus$text, remove_punct = TRUE)
refugee_dfm2 <- dfm(local_media_corpus$text, remove = stopwords("english"), remove_punct = TRUE)

# wordclouds
textplot_wordcloud(refugee_dfm1, max_words = 100)
textplot_wordcloud(refugee_dfm2, max_words = 100)

# TO DO:
# 1. send me your GitHub usernames, I will invite you to our GitHub folder and assign a respective folder to each
# 2. Download GitHub Desktop or leanr to use git from the terminal
# 3. Write your first script and add to our GitHub folder: explore other relevant keywords in context, focus on specific newspapers etc. 






