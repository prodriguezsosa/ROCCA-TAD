rm(list = ls())  # wipe clean

# TIPS:
# you should always (always!) annotate your code
# use version control (GitHub)
# DEBUGGING: rubber duck it
# Google is your friend. Type your question and add "R" to the end of it.
# for bigger projects: use a dependency manager (packrat)
# key packages: tidytext, quanteda, stringr

#-----------------------------
# 1 SETTING UP
#-----------------------------

# load libraries
library(dplyr)

# set working directory (where our data is stored)
setwd("~/Drobox/GitHub/Tennessee/data/")  # set working directory
getwd()  # returns current working directory

# load data
refugee_corpus_sentiment <- readRDS("refugee_corpus_sentiment.rds")

#-----------------------------
# QUICK VIEW OF THE DATA
#-----------------------------

# glimpse
glimpse(refugee_corpus_sentiment)
dim(refugee_corpus_sentiment)
nrow(refugee_corpus_sentiment)
ncol(refugee_corpus_sentiment)

# using the dollar sign to select specific variables
refugee_corpus_sentiment$headline[1:5] 
head(refugee_corpus_sentiment$headline)
tail(refugee_corpus_sentiment$headline)

# using matrix notation
refugee_corpus_sentiment[1:5, "headline"]
refugee_corpus_sentiment[1:5, c("headline", "sentiment")]

# using dplyr
refugee_corpus_sentiment %>% select(headline, sentiment) %>% slice(1:5)
refugee_corpus_sentiment %>% filter(source == "Williamson Source") %>% select(headline)

# table
table(refugee_corpus_sentiment$source)

# which newspapers have the most articles?
refugee_corpus_sentiment %>% group_by(source) %>% summarise(count = n()) %>% arrange(-count) %>% head  # pure count
refugee_corpus_sentiment %>% group_by(source) %>% summarise(count = n()) %>% mutate(freq = count/sum(count)) %>%  arrange(-freq) %>% head  # frequency

# face validity of sentiment measure
refugee_corpus_sentiment %>% arrange(-sentiment) %>% select(headline, sentiment) %>% head  # pure count
refugee_corpus_sentiment %>% arrange(sentiment) %>% select(headline, sentiment) %>% head  # pure count

# are spanish newspapers less negative?
refugee_corpus_sentiment %>% group_by(spanish) %>% summarise(mean_sentiment = mean(sentiment, na.rm = TRUE))

#-----------------------------
# CREATING VARIABLES
#-----------------------------
# dollar sign
refugee_corpus_sentiment$sentiment1 <- refugee_corpus_sentiment$positive - refugee_corpus_sentiment$negative

# matrix identifier
refugee_corpus_sentiment[, "sentiment2"]  <- refugee_corpus_sentiment[, "positive"] - refugee_corpus_sentiment[, "negative"]

# dplyr
refugee_corpus_sentiment <- refugee_corpus_sentiment %>% mutate(sentiment3 = positive - negative)

# should be the same
all.equal(refugee_corpus_sentiment$sentiment1, refugee_corpus_sentiment$sentiment2)
all.equal(refugee_corpus_sentiment$sentiment2, refugee_corpus_sentiment$sentiment3)

# drop variable
refugee_corpus_sentiment$sentiment3 <- NULL
"sentiment3" %in% colnames(refugee_corpus_sentiment)  # one way to check if deleted column was actually deleted
refugee_corpus_sentiment[, "sentiment2"] <- NULL  # using matrix notation
refugee_corpus_sentiment <- refugee_corpus_sentiment %>% select(-sentiment1)  # using dplyr

# create a date variable
library(lubridate)
refugee_corpus_sentiment <- refugee_corpus_sentiment %>% mutate(year = year(date))
table(refugee_corpus_sentiment$year)

#-----------------------------
# VISUALIZATION
#-----------------------------
# base R
barplot(table(refugee_corpus_sentiment$year), 
        main="Number of articles \n mentioning the word 'refugee'", 
        xlab="Year")

# ggplot2
library(ggplot2)
refugee_corpus_sentiment %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  arrange(year) %>% 
  na.omit() %>%  # two articles have no date assigned to them
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity") + 
  xlab("Year") + ylab("Number of articles \n mentioning the word 'refugee'") +
  theme_minimal()

#-----------------------------
# LOOPS, FUNCTIONS, APPLY
#-----------------------------
# functions
library(stringr)

count_tokens <- function(text){
  return(str_count(text, " ") + 1)
}

# loop
doc_length <- vector()
for(i in 1:nrow(refugee_corpus_sentiment)){
  doc_length[i] <- count_tokens(refugee_corpus_sentiment$headline[i])
}

# vectorized
doc_length2 <- count_tokens(refugee_corpus_sentiment$headline)

# use apply functions
doc_length3 <- lapply(refugee_corpus_sentiment$headline, count_tokens)

#-----------------------------
# REGULAR EXPRESSIONS
#-----------------------------
# regular expressions are a very powerful tool in wrangling text
# cheatsheet for regex: https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
# see other non-base packages e.g. stringr

# grep
children_index <- grep(" children ", refugee_corpus_sentiment$headline)
head(refugee_corpus_sentiment$headline[children_index])

# grepl
children_index <- grepl(" children ", refugee_corpus_sentiment$headline)
table(children_index)

# grepl
immigrant_index <- grepl("^Immigrant", refugee_corpus_sentiment$headline)
refugee_corpus_sentiment$headline[immigrant_index]

# this returns every speech that starts with the word Immigrant
grep("^Immigrant", refugee_corpus_sentiment$headline, value = TRUE)

# substitute words
gsub("Immigrant", "immigrant",  "Immigrant group blasts Tennessee bill to limit prenatal care")
grepl("^so", c("so today we", "never so today", "today never so"))

#-----------------------------
# PRE-PROCESSING
#-----------------------------
library(quanteda)
# other "similar" packages worth looking at: tm, tidytext

example <- refugee_corpus_sentiment$text[2]
example <- gsub("’", "", example) # remove all non-alpha characters
example <- gsub("[^[:alpha:]]", " ", example) # remove all non-alpha characters
example <- tolower(example)  # lowercase
example <- str_replace_all(example, "^ +| +$|( ) +", "\\1")  # remove excess white space
length(unique(unlist(tokens(example))))
example <- example %>% tokens() %>% tokens_wordstem() %>% paste(collapse = " ")  # language is an argument
length(unique(unlist(tokens(example))))

#-----------------------------
# KEY WORDS IN CONTEXT
#-----------------------------
# explore uses of a given word (kwic = (k)ey (w)ord (i)n (c)ontext)
kwic_children <- kwic(refugee_corpus_sentiment$text, pattern = "children", window = 10)

#-----------------------------
# WORDCLOUD
#-----------------------------
# quanteda requires we create a document feature matrix (will discuss next time)
refugee_dfm1 <- dfm(refugee_corpus_sentiment$text, remove_punct = TRUE)
refugee_dfm2 <- dfm(refugee_corpus_sentiment$text, remove = stopwords("english"), remove_punct = TRUE)

# wordclouds
textplot_wordcloud(refugee_dfm1, max_words = 100)
textplot_wordcloud(refugee_dfm2, max_words = 100)
