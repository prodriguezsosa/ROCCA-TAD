rm(list = ls())
# https://www.allyoucanread.com/tennessee-newspapers/ Tennessee Media
# https://www.youtube.com/watch?v=HXV3zeQKqGY  SQL primer
# https://www.gdeltproject.org/data/lookups/  GDELT codes
# https://www.gdeltproject.org/data/lookups/CAMEO.eventcodes.txt
# EventCode = 0233 = Appeal for humanitarian aid
# EventCode = 0333 = Express intent to provide humanitarian aid

# --------------------------------
# setup
# --------------------------------

# initialize environment
packrat::init("~/Dropbox/GitHub/environments/ROCCA-TAD/")
packrat::on()

# libraries
library(dplyr)
library(stringr)
library(pbapply)
library(ggplot2)
library(devtools)
library(quanteda.dictionaries)
library(quanteda)
library(treemap)

# load data
corpus <- readRDS("/Volumes/Potosi/Research/ROCCA/gdelt/tennessee_media.rds")
dictionaries <- "~/Dropbox/GitHub/repositories/voice2insights/data/dictionaries/"

# subset corpus
corpus <- corpus %>% 
  select(SQLDATE, MonthYear, Year, NumMentions, NumSources, NumArticles, AvgTone, SOURCEURL, MentionSourceName, MentionIdentifier, Confidence, MentionDocLen, MentionDocTone) %>% 
  filter(Year > 2014) %>% 
  distinct(MentionIdentifier, .keep_all = TRUE)

# add location data


# get relevant headlines from URLs
keyterms <- c('refugee', 'asyl', 'immig', 'migrant')
find_headlines <- function(sourceurl){
  headline <- unlist(str_split(sourceurl, '/'))
  reg1 <- grep(paste(keyterms, collapse = '|'), headline, ignore.case = TRUE)
  reg2 <- grep('-', headline)
  regexp_present <- intersect(reg1, reg2)
  if(length(regexp_present)==1){headline <- headline[regexp_present]}else{headline <- NA}
  return(headline)
}

corpus$headlines  <- pblapply(corpus$SOURCEURL, find_headlines) %>% unlist()

# frequency
corpus <- corpus %>% mutate(on_topic = if_else(is.na(headlines), 0, 1))
corpus %>% 
  select(Year, on_topic) %>% 
  group_by(Year) %>% 
  summarise(total = n(), on_topic = sum(on_topic)) %>% 
  mutate(proportion = (on_topic/total)*100) %>% 
  na.omit() %>%
  ggplot(aes(x = as.character(Year), y = proportion)) + 
  geom_bar(position = "dodge", stat = "identity", color = '#F8766D', fill = '#619CFF') +
  xlab("") + 
  ylab("Percent of Articles on Topic") + 
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

# avg tone by newspaper
plot_tibble <- corpus %>% 
  select(MentionSourceName, on_topic, AvgTone, MentionDocTone) %>% 
  group_by(MentionSourceName) %>% 
  summarise(mean_tone = mean(MentionDocTone, na.rm = TRUE), std.error = sd(MentionDocTone, na.rm = TRUE)/sqrt(n())) %>%
  mutate(MentionSourceName = gsub('.[[:alpha:]]+$', '', MentionSourceName)) %>%
  arrange(-mean_tone) %>%
  mutate(MentionSourceName = factor(MentionSourceName, levels = unique(MentionSourceName))) %>%
  na.omit()

ggplot(plot_tibble, aes(x = MentionSourceName, y = mean_tone)) +  
  geom_point(size = 2, color = if_else(plot_tibble$mean_tone <= 0, 'red', 'blue')) +
  geom_errorbar(aes(ymin = mean_tone + 1.96*std.error, ymax = mean_tone - 1.96*std.error), width=.2,position=position_dodge(0.05)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black", size = 0.5) + 
  ylab("Avg. Tone By Newspaper") +
  xlab("") +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=18),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "top",
        legend.spacing.x = unit(0.25, 'cm'))

# avg tone by year (plot loess curve by month or boxplot by year)
plot_tibble <- corpus %>% 
  select(MonthYear, on_topic, AvgTone, MentionDocTone) %>% 
  group_by(MonthYear) %>% 
  summarise(mean_tone = mean(MentionDocTone, na.rm = TRUE), std.error = sd(MentionDocTone, na.rm = TRUE)/sqrt(n())) %>%
  arrange(-mean_tone) %>%
  na.omit()

ggplot(plot_tibble, aes(x = MonthYear, y = mean_tone)) + 
  geom_point(size = 2, color = if_else(plot_tibble$mean_tone <= 0, 'red', 'blue')) +
  ylab("Avg. Tone By Newspaper") +
  xlab("") +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=18),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "top",
        legend.spacing.x = unit(0.25, 'cm'))



# moral foundations

mfdict <- dictionary(file = paste0(dictionaries, "moral_foundations_dictionary.dic"), format = "LIWC")

# dictionary analysis
mf_dfm <- dfm(corpus$headlines, dictionary = mfdict, groups = corpus$Year) %>% convert('matrix')
groups <- rownames(mf_dfm)
foundations <- colnames(mf_dfm)
mf_tibble <- lapply(groups, function(i) tibble(group = i, foundation = foundations, value = unname(mf_dfm[i,]))) %>% do.call(rbind,.)
mf_tibble <- mf_tibble %>% mutate(foundation_agg = gsub(paste0(c('Vice', 'Virtue'), collapse = '|'),'', foundation))
mf_tibble <- mf_tibble %>% group_by(group) %>% mutate(value_prop = value/sum(value)) %>% arrange(group, -value_prop) %>% ungroup()

# https://www.r-graph-gallery.com/treemap.html
#http://www.jeremyfrimer.com/uploads/2/1/2/7/21278832/summary.pdf
treemap(mf_tibble[mf_tibble$group == 2019,],
        index="foundation",
        vSize="value",
        type="index"
)

# sentiment analysis





