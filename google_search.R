library(jsonlite)
library(dplyr)
library(purrr)
library(tm)
library(wordcloud)
library(wesanderson)
library(ggplot2)
library(lubridate)

#First I want to read in all the json files I have downloaded from my 
#Google search history

my_files <- list.files(path = './data/')
searches <- list()
for (file in my_files)  {
    searches <- c(searches, read_json(paste0("data/", as.character(file))))
}



search_terms <- searches$event %>%
    map(function(e) { e$query$query_text }) 
   
    

#and collect the query text from each search
search_all_query_text <-
    searches$event %>%
    map(function(e) { e$query$query_text }) %>%
    reduce(function(all_text, query_text) { paste(all_text, query_text) })

# # #collect dates from each search
# # timestamps <-
# #     searches$event %>%
# #     map(function(e) { e$query$id$timestamp_usec }) %>%
# #     reduce(function(all_dates, timestamp_usec) { paste(all_dates, timestamp_usec) })
# 
# #dates were harder; there's an unamed node (child of id & parent of timestamp_usec)
# #unlisting put search terms in even numbered positions and dates in odd-numbered positions


#kind of hacky - non-dates fall out as NAs when coerced to date format
unlisted_searches <- unlist(searches)
dates <- as.POSIXct((as.numeric(unlisted_searches)/1000000), origin="1970-01-01")
datemask <- !is.na(dates)
dates_decoded <- dates[datemask]
 

# #dates <- timestamps <- sapply(unlisted_searches, '[', 'event.query.id.timestamp_usec')
# 
# #then just grab the odd-numbered elements
# dates <- list()
# for (i in 1:length(unlisted_searches))  {
#     if (!(i %% 2 == 0))  {
#         dates <- c(dates, unlisted_searches[[i]])
#     }
# }
# 
# #convert from unix epoch
# dates_decoded <-as.POSIXct((as.numeric(dates)/1000000), origin="1970-01-01")

#sum(is.na(dates_decoded))
# times <- !(is.na(dates_decoded))
# search_times <- dates_decoded[times]
datetimes <- ymd_hms(dates_decoded)
dates <- as.Date(datetimes, format = "%Y-%m-%d")
#times <-  as.Date(datetimes, format = "%H-%M-%S")

date_df <- as.data.frame(dates)
date_df$month <- month(date_df$dates) 
date_df$day <- day(date_df$dates)
date_df$wday <- wday(date_df$dates)

save(date_df, file = "data/dates.rda")

by_month <- date_df %>%
    group_by(month) %>%
    summarise(count = n())
ggplot(by_month, aes(month, count, 
                     label = c('Jan ', 'Feb ', 'Mar ', 'Apr ', 'May ', 'Jun ', 
                               'Jul ', 'Aug ', 'Sep ', 'Oct ', 'Nov ', 'Dec '))) +
    geom_col( fill = 'darkgreen') +
    scale_x_discrete('month') +
    geom_text(colour = 'white', angle = 90, hjust = 1) + 
    ggtitle('Number of Google Searches by Month 2016 & 2017')


by_wday <- date_df %>%
    group_by(wday) %>%
    summarise(count = n())
ggplot(by_wday, aes(wday, count, 
                     label = c('Sunday ', 'Monday ', 'Tuesday ', 'Wednesday ', 
                               'Thursday ', 'Friday ', 'Saturday '))) +
    geom_col( fill = 'darkgreen') +
    scale_x_discrete('') +
    geom_text(colour = 'white', angle = 90, hjust = 1) +
    ggtitle('Number of Google Searches by Day of Week 2016 & 2017')



dates_summary <- date_df %>% 
    group_by(as.character(dates)) %>%
    summarise(count = n())

names(dates_summary) <- c('date', 'count')
# ggplot(dates_summary, aes(date, count)) +
#     geom_col()

save(search_all_query_text, file = "data/searches.rda")

corp1 <- Corpus(VectorSource(search_all_query_text))
# corp1 <- tm_map(corp1, PlainTextDocument)

corp1 <- tm_map(corp1, tolower)
corp1 <- tm_map(corp1, removePunctuation)
corp1 <- tm_map(corp1, removeWords, stopwords("english"))
dtm <- DocumentTermMatrix(corp1)
freq <- colSums(as.matrix(dtm))
ordered_freq <- order(freq)
top_20 <- freq[tail(ordered_freq, n=20)]


wordcloud(dtm$dimnames$Terms, freq, min.freq = 3, max.words = 200)
wordcloud(dtm$dimnames$Terms, freq, min.freq = 3, random.order = TRUE, colors = wes_palette(name = "BottleRocket"))

