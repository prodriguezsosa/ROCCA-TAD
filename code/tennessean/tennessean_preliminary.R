# --------------------------------
# setup
# --------------------------------

# libraries
library(stringr)
library(dplyr)
library(quanteda)
library(wordcloud)
library(stringr)
library(ggplot2)
library(lubridate)

# paths
data_path <- "~/Dropbox/GitHub/large_data/ROCCA-TAD/tennessean/processed/" # path to processed tennessean data
figure_path <- "~/Dropbox/GitHub/repositories/ROCCA-TAD/code/figures/"

# load data
corpus <- readRDS(paste0(data_path, 'tennessean.rds'))

# extract year from date for cases in which the year variable is missing
corpus <- corpus %>% mutate(year2 = str_extract(date, "[[:digit:]]{4}$"))
corpus$year[is.na(corpus$year) & !is.na(corpus$year2)] <- corpus$year2[is.na(corpus$year) & !is.na(corpus$year2)] 
corpus <- corpus %>% mutate(year = gsub(' ', '', year)) # some of the observations had spacing issue

# preprocess text
corpus$full_text <- gsub("[^[:alpha:]]", " ", corpus$full_text) # remove all non-alpha characters
corpus$full_text<- str_replace_all(corpus$full_text, "\\b\\w{1,2}\\b", "") # remove 1-2 letter words
corpus$full_text<- str_replace_all(corpus$full_text, "^ +| +$|( ) +", "\\1")  # remove excess white space
corpus$full_text <- tolower(corpus$full_text)  # lowercase
corpus <- corpus[corpus$full_text!="",] # remove nuls

# keyterms
keyterms <- c('refugee', 'asyl', 'immig', 'migrant')
articles_on_topic <- grep(paste(keyterms, collapse = '|'), corpus$full_text, ignore.case = TRUE)
corpus <- corpus %>% mutate(on_topic = if_else(grepl(paste(keyterms, collapse = '|'), full_text, ignore.case = TRUE), 1, 0))

# wordcloud
corpus_dfm <- dfm(corpus$full_text[corpus$on_topic == 1], remove = stopwords('english'), remove_punct = TRUE, tolower = TRUE)
words <- sort(colSums(corpus_dfm), decreasing=TRUE) 
df <- data.frame(word = names(words),freq = words)
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

# tf-idf wordcloud
exclude <- c('immigration', 'immigrants', 'immigrant', 'refugee', 'refugees')
corpus_dfm <- dfm(corpus$full_text, remove = c(stopwords('english'), exclude), remove_punct = TRUE, tolower = TRUE)
corpus_dfm <- dfm_tfidf(corpus_dfm, scheme_tf = 'prop', scheme_df = 'inversemax')
corpus_dfm <- corpus_dfm[articles_on_topic,]
words <- sort(colSums(corpus_dfm),decreasing=TRUE) 
df <- data.frame(word = names(words),freq = words)
set.seed(04092020)
png(file=paste0(figure_path, "the_tennessean_wc.png"))
wordcloud(words = df$word, freq = df$freq, scale=c(2,0.5), min.freq = 1, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
dev.off()

# line plot
plot_tibble <- corpus %>% select(year, on_topic) %>% group_by(year) %>% summarise(total = n(), on_topic = sum(on_topic)) %>% mutate(proportion = (on_topic/total)*100) %>% na.omit()

png(file=paste0(figure_path, "the_tennessean_freq.png"))
ggplot(plot_tibble, aes(x = as.character(year), y = proportion)) + 
  geom_bar(position = "dodge", stat = "identity", color = '#F8766D', fill = '#619CFF') +
  xlab("") + 
  ylab("Percent of Articles on Topic of Immigration/Refugees") + 
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size=15, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size=15),
    axis.title.y = element_text(size=18, margin = margin(t = 0, r = 20, b = 0, l = 20)),
    axis.title.x = element_text(size=18, margin = margin(t = 20, r = 0, b = 20, l = 0)))
dev.off()

