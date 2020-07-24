rm(list = ls()) 

# --------------------------------
# setup
# --------------------------------
packrat::init("~/Dropbox/GitHub/environments/ROCCA-TAD/")
packrat::on()

# libraries
library(readtext)
library(stringr)
library(dplyr)
library(pbapply)
library(progress)

# paths
data_path <- "~/Dropbox/Research/Tennessee/ROCCA-TAD/"
out_path <- "~/Dropbox/Research/Tennessee/ROCCA-TAD/processed/"

# load data
scraper_names <- c('ancher-2015', 'angelina-2018', 'courtenay-2019', 'holly-2014', 'mark-2017', 'monika-2016')

scraper_list <- vector('list', length(scraper_names)) %>% setNames(scraper_names)
for(scraper in scraper_names){
  path_subfile <- paste0(data_path, scraper)
  files_list <- as.list(list.files(path_subfile))
  
  # split document into articles
  
  document_list <- vector('list', length(files_list))
  pb <- progress_bar$new(total = length(files_list))
  for(j in 1:length(files_list)){
    file_i <- readtext(paste(path_subfile, files_list[[j]], sep = '/'))
    file_i <- unlist(str_split(file_i$text, '____________________________________________________________'))  # demarcates articles
    file_i <- file_i[!file_i=='']
    # for each article
    article_list <- vector('list', length(file_i))
    for(i in 1:length(file_i)){
      article_i <- unlist(str_split(file_i[i], '\n'))
      article_i <- article_i[!article_i=='']
      
      # full text
      if(!any(grepl('Terms and Conditions:', article_i))){
        full_text_start <- grep(pattern = "Full text:", article_i)
        speakers_index <- grep(pattern = "\\:.*", article_i)
        full_text_end <- speakers[which(speakers_index == full_text_start) + 1]
        full_text = article_i[full_text_start:full_text_end] %>% 
          paste0(collapse = ' ') %>% 
          gsub('\"', '', .) %>%
          str_trim('both') %>% 
          str_squish() %>% 
          gsub('Full text: ', '',.)
        # metadata
        abstract = article_i[grep('Abstract:', article_i)] %>% gsub('Abstract: ', '',.)
        subjects = article_i[grep('Subject:', article_i)] %>% gsub('Subject: ', '',.)
        title = article_i[grep('Title:', article_i)] %>% gsub('Title: ', '',.)
        year = article_i[grep('Publication year:', article_i)] %>% gsub('Publication year: ', '',.)
        date = article_i[grep('Publication date:', article_i)] %>% gsub('Publication date: ', '',.)
        publisher = article_i[grep('Publisher:', article_i)] %>% gsub('Publisher: ', '',.)
        place_of_publication = article_i[grep('Place of publication:', article_i)] %>% gsub('Place of publication: ', '',.)
        country_of_publication = article_i[grep('Country of publication:', article_i)] %>% gsub('Country of publication: ', '',.)
        subject_of_publication = article_i[grep('Publication subject:', article_i)] %>% gsub('Publication subject: ', '',.)
        issn = article_i[grep('ISSN:', article_i)] %>% gsub('ISSN: ', '',.)
        source_type = article_i[grep('Source type:', article_i)] %>% gsub('Source type: ', '',.)
        language = article_i[grep('Language of publication:', article_i)] %>% gsub('Language of publication: ', '',.)
        document_type = article_i[grep('Document type:', article_i)] %>% gsub('Document type: ', '',.)
        proquest_id = article_i[grep('ProQuest document ID:', article_i)] %>% gsub('ProQuest document ID: ', '',.)
        url = article_i[grep('Document URL:', article_i)] %>% gsub('Document URL: ', '',.)
        copyright = article_i[grep('Copyright:', article_i)] %>% gsub('Copyright: ', '',.)
        database = article_i[grep('Database:', article_i)] %>% gsub('Database: ', '',.)
        
        article_i <- tibble(full_text = ifelse(length(full_text) == 1, full_text, NA),
                            abstract = ifelse(length(abstract) == 1, abstract, NA),
                            subjects = ifelse(length(subjects) == 1, subjects, NA),
                            title = ifelse(length(title) == 1, title, NA),
                            year = ifelse(length(year) == 1, year, NA),
                            date = ifelse(length(date) == 1, date, NA),
                            publisher = ifelse(length(publisher) == 1, publisher, NA),
                            place_of_publication = ifelse(length(place_of_publication) == 1, place_of_publication, NA),
                            country_of_publication = ifelse(length(country_of_publication) == 1, country_of_publication, NA),
                            subject_of_publication = ifelse(length(subject_of_publication) == 1, subject_of_publication, NA),
                            issn = ifelse(length(issn) == 1, issn, NA),
                            source_type = ifelse(length(source_type) == 1, source_type, NA),
                            language = ifelse(length(language) == 1, language, NA),
                            document_type = ifelse(length(document_type) == 1, document_type, NA),
                            proquest_id = ifelse(length(proquest_id) == 1, proquest_id, NA),
                            url = ifelse(length(url) == 1, url, NA),
                            copyright = ifelse(length(copyright) == 1, copyright, NA),
                            database = ifelse(length(database) == 1, database, NA))
        
        article_list[[i]] <- article_i}
    }
    document_list[[j]] <- do.call(rbind, article_list)
    pb$tick()
  }
  scraper_list[[scraper]] <- do.call(rbind, document_list)
  }

tennessean <- do.call(rbind, scraper_list)

# some extra cleaning
corpus$year <- as.integer(gsub(' ', '', corpus$year))
corpus$date <- as.Date(corpus$date,format="%b %d, %Y")
corpus$document_type <- gsub(' ', '', corpus$document_type)
corpus$publisher[grepl('^G', corpus$publisher)] <- 'Gannett Co., Inc.'
corpus$language[grepl('^E', corpus$language)] <- 'English'
corpus$source_type[grepl(paste('^N', '^n', '|'), corpus$source_type)] <- 'Newspapers'


saveRDS(tennessean, paste0(out_path, "tennessean.rds"))






