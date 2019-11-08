rm(list = ls())  # wipe clean

# TIPS:
# you should always (always!) annotate your code
# use version control (GitHub)
# DEBUGGING: rubber duck it
# Google is your friend. Type your question and add "R" to the end of it.
# for bigger projects: use a dependency manager (packrat)

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


















#-----------------------------
# 2 WORKING WITH DATA
#-----------------------------

# 2.1 Take a peek, get to know the structure of the data

head(refugee_corpus_sentiment)  # display first lines of an object
tail(refugee_corpus_sentiment)  # display last lines of an object
str(refugee_corpus_sentiment)  # display structure of an R object (e.g. a dataframe)
glimpse(refugee_corpus_sentiment)
dim(refugee_corpus_sentiment)  # data dimensions
nrow(refugee_corpus_sentiment)  # number of rows
ncol(refugee_corpus_sentiment)  # number of columns
colnames(refugee_corpus_sentiment)  # column names
rownames(refugee_corpus_sentiment) # row names
class(refugee_corpus_sentiment)  # returns class of an R object
sapply(refugee_corpus_sentiment, class) # returns class for each variable (column)

# 2.2 Subset dataframes ----------------------------------------------------

# A) Get column with dollar sign operator
head(refugee_corpus_sentiment$headline)

# B) Matrix identifier: df[rowname, colname]
refugee_corpus_sentiment[, "headline"]

# Alternatively:
View(refugee_corpus_sentiment[, c("source", "headline")])

# C) dplyr
# a very powerful package with intuitive commands for subsetting, selecting, and transforming your data
# https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html  # cran documentation
# https://r4ds.had.co.nz/tidy-data.html  # going beyond dplyr, tidy data principles
# https://www.tidyverse.org  # other packages in the tidy universe (tidyverse)
# note: it's always useful to be able to do things with base R functions (helps understanding)

# Using pipe notation
refugee_corpus_sentiment %>% select(source) %>% head(10)
refugee_corpus_sentiment %>% select(source, headline) %>% head()

# Alternative syntax
head(select(refugee_corpus_sentiment, source, headline)) # stick to one syntax, repetition helps recall (note order matters)

# 2.3 How to locate row(s) in a data frame ----------------------------------------------------

# A) Dollar sign operator
refugee_corpus_sentiment$headline[1] # Returns the first row of the data frame in the specified column (Python users: R indexing starts at 1)
refugee_corpus_sentiment$headline[1:5] # Returns the first 5 rows of the data frame in the specified column
refugee_corpus_sentiment$headline[refugee_corpus_sentiment$source == "Williamson Source"] # Returns all rows for the variable "headline" where source = Williamson Source

# B) Column name
refugee_corpus_sentiment[1, "headline"] 
refugee_corpus_sentiment[1:5, "headline"] 
refugee_corpus_sentiment[refugee_corpus_sentiment$source == "Williamson Source", "headline"] 

# C) dplyr
# Pipe syntax
refugee_corpus_sentiment %>% slice(1) %>% select(headline) 
refugee_corpus_sentiment %>% slice(1:5) %>% select(headline)
refugee_corpus_sentiment %>% filter(source == "Williamson Source") %>% select(headline)
refugee_corpus_sentiment %>% filter(source == "Williamson Source") %>% slice(1) %>% select(headline)  # can keep "piping"

# 2.4 Creating new variables (columns) in a data frame ----------------------------------------------------

# A) Dollar sign operator
refugee_corpus_sentiment$sentiment1 <- refugee_corpus_sentiment$positive - refugee_corpus_sentiment$negative

# B) Matrix identifier
refugee_corpus_sentiment[, "sentiment2"]  <- refugee_corpus_sentiment[, "positive"] - refugee_corpus_sentiment[, "negative"]

# C) dplyr
# Pipe syntax
refugee_corpus_sentiment <- refugee_corpus_sentiment %>% mutate(sentiment3 = positive - negative)

# Are these variables equivalent to one another?
all.equal(refugee_corpus_sentiment$sentiment1, refugee_corpus_sentiment$sentiment2)  
all.equal(refugee_corpus_sentiment$sentiment2, refugee_corpus_sentiment$sentiment3) 

# 2.5 Removing columns ----------------------------------------------------
refugee_corpus_sentiment$sentiment3 <- NULL
"sentiment3" %in% colnames(refugee_corpus_sentiment)  # one way to check if deleted column was actually deleted
refugee_corpus_sentiment[, "sentiment2"] <- NULL  # using matrix notation

# Using dplyr
refugee_corpus_sentiment <- refugee_corpus_sentiment %>% select(-sentiment1)
refugee_corpus_sentiment <- refugee_corpus_sentiment %>% select(-c(oped, url))

# 2.6 Summarizing Data ----------------------------------------------------

# A) Start always by getting to know the structure of the data (see above)

# B) General summary
summary(refugee_corpus_sentiment)  # summary statistics where appropriate (non-string/character variables)

# C) Single variable summary
mean(refugee_corpus_sentiment$sentiment, na.rm = TRUE)
sd(refugee_corpus_sentiment$sentiment, na.rm = TRUE)
refugee_corpus_sentiment %>% summarise(mean_sentiment = mean(sentiment, na.rm = TRUE))  # using dplyr
refugee_corpus_sentiment %>% filter(source == "Williamson Source") %>% summarise(mean_sentiment = mean(sentiment, na.rm = TRUE))  # summary for a specific group

# D) Summary by group
refugee_corpus_sentiment %>% group_by(spanish) %>% summarise(mean_sentiment = mean(sentiment, na.rm = TRUE))  # use group_by
refugee_corpus_sentiment %>% group_by(spanish) %>% summarise(mean_sentiment = mean(sentiment, na.rm = TRUE), sd_sentiment = sd(sentiment, na.rm = TRUE))  # can perform multiple summary stats
table1 <- refugee_corpus_sentiment %>% group_by(source, spanish) %>% summarise(mean_sentiment = mean(sentiment, na.rm = TRUE)) %>% ungroup %>% slice(1:5)  # can group by more than one variable

View(table1)

# E) Summarizing a variable with a histogram

# Basic R graphics
hist(refugee_corpus_sentiment$sentiment)

# ggplot2 graphics
plot1 <- ggplot(aes(sentiment), data = refugee_corpus_sentiment) + geom_histogram(bins = 15) + theme_light()

plot1

# take a look at plotly for interactive plots: https://plot.ly/r/

# 2.7 Exporting data

# Exporting table to CSV
write.csv(table1,file = "table1.csv")

# Creating LaTeX table (copy output and paste in your Latex document)
xtable(table1,caption = "Average Clinton Polling Advantage by Polling Firm")

stargazer(table1, summary = FALSE)

# Exporting graph to pdf
pdf(width = 4, height = 3, "plot1.pdf")
plot1
dev.off()




















# extract year
refugee_corpus <- refugee_corpus %>% mutate(year = year(date))

# plot count by year
refugee_corpus %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  arrange(year) %>% 
  na.omit() %>%  # two articles have no date assigned to them
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity") + 
  xlab("Year") + ylab("Number of articles \n mentioning the word 'refugee'") +
  theme_minimal()

# which newspapers have the most articles?
refugee_corpus %>% group_by(source) %>% summarise(count = n()) %>% arrange(-count) %>% head  # pure count
refugee_corpus %>% group_by(source) %>% summarise(count = n()) %>% mutate(freq = count/sum(count)) %>%  arrange(-freq) %>% head  # frequency

# regular expressions














# quanteda
library(quanteda)

# explore uses of a given word (kwic = (k)ey (w)ord (i)n (c)ontext)
kwic_hispanic <- kwic(refugee_corpus$text, pattern = "hispanic", window = 10)


# functions
library(stringr)

count_tokens <- function(text){
  return(str_count(text, " ") + 1)
}

refugee_corpus <- refugee_corpus %>% mutate(num_tokens = count_tokens(text))
refugee_corpus <- refugee_corpus %>% mutate(num_tokens2 = ntoken(text))
all.equal(refugee_corpus$num_tokens2, refugee_corpus$num_tokens)
cor(refugee_corpus$num_tokens, refugee_corpus$num_tokens2)

# loops

# document feature matrix

# tidytext
library(tidytext)
refugee_corpus_dfm <- dfm(refugee_corpus$title)
refugee_corpus_tidy <- tidy(refugee_corpus_dfm)

# sentiment analysis
refugee_sentiment <- refugee_corpus_tidy %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))


library(tidyr)

refugee_sentiment %>%
  count(document, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(-sentiment)

refugee_corpus$title[1010]
refugee_corpus$title[1056]

library(ggplot2)

refugee_sentiment %>%
  count(sentiment, term, wt = count) %>%
  filter(n >= 2) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")

# loading libraries etc
# quanteda vs. tidytext
# regular expressions
# dfms
# basic data manipulation
# word clouds
# sentiment analysis