# ROCCA-TAD

Below we describe the data and R scripts put together by the ROCCA text as data team. You can access the data using [this link](https://www.dropbox.com/sh/jrcvnsmpgdtinul/AAC_Kq6PRhZQaZf4HDPsG52da?dl=0).

Available corpora:

**1. [The Tennessean](https://www.tennessean.com):**

**a. Data:** 
- Type of text: full collection of News articles (includes full article texts)
- Period: 2014 - 2019
- Source: [ProQuest News and Newspapers](https://about.proquest.com/products-services/news-newspapers/)


**b. Scripts:**
- clean_tennessean.R: processes all the raw txt datafiles collected by the team and outputs tennessean.rds.
- tennessean_preliminary.R: performs some basic analysis on tennessean.rds (topic wordcloud and topic frequency plot).

![the_tennessean_freq.png](https://github.com/prodriguezsosa/ROCCA-TAD/blob/master/figures/the_tennessean_freq.png?raw=true)
![the_tennessean_wc.png](https://github.com/prodriguezsosa/ROCCA-TAD/blob/master/figures/the_tennessean_wc.png?raw=true)

**1. [GDELT]:**

**a. Data:** 
- Type of text: urls and tone of all articles (scraped by GDELT) for the top 32 newspapers in TN (should have at least one per our locations of interest).
- Period: 2015 - March 2020
- Source: [The GDELT Project](https://www.gdeltproject.org)
- Note: the urls for the most part are similar to headlines. For a full description of all the variables see the links referred to in the scripts.

**b. Scripts:**
- gdelt_scraper.R: scrapes GDELT for the top Tennessean newspapers in each of our target locations and outputs tennessee_media.rds.
- clean_gdelt.R: pre-processes tennessee_media.rds and performs basic analysis on tennessee_media.rds (topic frequency plot and average tone by newspaper).

![gdelt_freq.png](https://github.com/prodriguezsosa/ROCCA-TAD/blob/master/figures/gdelt_freq.png?raw=true)
![gdelt_tone.png](https://github.com/prodriguezsosa/ROCCA-TAD/blob/master/figures/gdelt_tone.png?raw=true)

**1. [NOW Corpus]:**

**a. Data:** 
- Type of text: collection of full text US news articles ([see here](https://www.english-corpora.org/now/) for description).
- Period: 2010 - 2019
- Source: [News on the Web (NOW) Corpus](https://www.english-corpora.org/now/)

**b. Scripts:**
- prepare_now_data.R: cleans raw NOW data and outputs news_corpus.rds.
