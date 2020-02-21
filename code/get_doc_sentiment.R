rm(list = ls())

# --------------------------------
# initialize environment
# --------------------------------
packrat::init("~/Dropbox/GitHub/environments/ROCCA-TAD/")
packrat::on()

# load libraries
library(dplyr)
library(tidytext)
library(tidyr)
library(quanteda)

# define data path
in_path <- "~/Drobox/GitHub/Tennessee/data/"

# load data
refugee_corpus <- readRDS(paste0(in_path, "corpus_of_articles.rds"))

# convert to tidy format
corpus_tidy <- refugee_corpus %>% 
  select(document, text) %>% 
  unnest_tokens(word, text)
  
# count word occurrences by document
corpus_tidy <- corpus_tidy %>% 
  group_by(document, word) %>% 
  mutate(count = n()) %>%
  ungroup()

# attach sentiment to each word
corpus_tidy <- corpus_tidy %>%
  inner_join(get_sentiments("nrc"), by = "word")

# collapse by document
corpus_tidy <- corpus_tidy %>%
  count(document, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(-sentiment)

# merge with original corpus & compute average sentiment
refugee_corpus_sentiment <- left_join(refugee_corpus, corpus_tidy, by = "document") %>% mutate(sentiment = sentiment/ntoken(text))

# save
saveRDS(refugee_corpus_sentiment, paste0(in_path, "corpus_of_articles_sentiment.rds"))
