# useful urls:
# https://www.allyoucanread.com/tennessee-newspapers/ Tennessee Media
# https://www.youtube.com/watch?v=HXV3zeQKqGY  SQL primer
# https://www.gdeltproject.org/data/lookups/  GDELT codes

# --------------------------------
# setup
# --------------------------------

# libraries
library(dplyr)
library(bigrquery)
library(tidyr)

#-------------------------------
# using SQL
#-------------------------------
billing <- "" # replace this with your project ID 
sql <- "WITH T1 AS (
  SELECT DISTINCT GLOBALEVENTID, MentionType, MentionSourceName, MentionIdentifier, SentenceID, Confidence, MentionDocLen, MentionDocTone
  FROM `gdelt-bq.gdeltv2.eventmentions`
  WHERE MentionSourceName IN ('tennessean.com', 'knoxnews.com', 'wbir.com', 'newschannel5.com', 'timesfreepress.com', 
  'wate.com', 'wsmv.com', 'wkrn.com', 'commercialappeal.com', 'wrcbtv.com', 'clevelandbanner.com', 'oakridger.com',
  'thedailytimes.com', 't-g.com', 'germantownnews.com', 'dnj.com', 'chattanoogan.com', 'nashvillescene.com',
  'local8now.com', 'wbbjtv.com', 'newschannel9.com', 'columbiadailyherald.com', 'nashvillepost.com', 'johnsoncitypress.com',
  'memphisdailynews.com', 'greenevillesun.com', 'timesnews.net', 'theleafchronicle.com', 'memphisflyer.com',
  'jacksonsun.com', 'herald-citizen.com', 'christianexaminer.com', 'nashvillelifestyles.com')
)
SELECT `gdelt-bq.gdeltv2.events`.*, T1.MentionType, T1.MentionSourceName, T1.MentionIdentifier, T1.SentenceID, T1.Confidence, T1.MentionDocLen, T1.MentionDocTone
#SELECT T1.*, `gdelt-bq.gdeltv2.events`.Year, `gdelt-bq.gdeltv2.events`.Actor1Code
#SELECT T1.*
FROM `gdelt-bq.gdeltv2.events`
RIGHT JOIN T1 ON `gdelt-bq.gdeltv2.events`.GLOBALEVENTID = T1.GLOBALEVENTID
ORDER BY GLOBALEVENTID DESC;"

tb <- bq_project_query(billing, sql)
test_extract <- bq_table_download(tb)
saveRDS(test_extract, "~/Dropbox/GitHub/large_data/ROCCA-TAD/gdelt/tennessee_media.rds")


