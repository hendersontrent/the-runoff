#--------------------------------------------
# This script sets out to scrape Twitter
# data related to the #AustraliaDay tag
#--------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 19 January 2021
#------------------------------------------

library(rtweet)
library(twitteR)
library(tidytext)

#----------------------- Setup Twitter dev creds -------------------

source("twitter-auth/twitter-creds.R") # Not pushed to git due to privacy

#----------------------- Extract tweets ----------------------------

#' Function to pull tweets based on a specific hashtag
#' 
#' @param hashtag the hashtag to filter the Twitter API for
#' @return a dataframe of scraped tweets
#' @author Trent Henderson
#' 

pull_tweets <- function(hashtag){
  
  # Pull tweets and add some useful flags as columns
  
  tweets <- search_tweets(q = hashtag, n = 3200, lang = "en") %>%
    mutate(hashtag = hashtag) %>%
    mutate(flag = case_when(
           grepl("#AustraliaDay", text) & !grepl("#InvasionDay", text) & !grepl("#changethedate", text) ~ "#AustraliaDay only",
           grepl("#InvasionDay", text) & !grepl("#AustraliaDay", text) & !grepl("#changethedate", text) ~ "#InvasionDay only",
           grepl("#changethedate", text) & !grepl("#AustraliaDay", text) & !grepl("#InvasionDay", text) ~ "#changethedate only",
           grepl("#AustraliaDay", text) & grepl("#InvasionDay", text) & !grepl("#changethedate", text)  ~ "#AustraliaDay and #InvasionDay",
           grepl("#AustraliaDay", text) & !grepl("#InvasionDay", text) & grepl("#changethedate", text)  ~ "#AustraliaDay and #changethedate",
           !grepl("#AustraliaDay", text) & grepl("#InvasionDay", text) & grepl("#changethedate", text)  ~ "#InvasionDay and #changethedate",
           grepl("#AustraliaDay", text) & grepl("#InvasionDay", text) & grepl("#changethedate", text)   ~ "#AustraliaDay, #InvasionDay, and #changethedate"))
  
  return(tweets)
}

# Run the function for relevant hashtags of interest

hashes <- c("#AustraliaDay", "#InvasionDay", "changethedate")
storage <- list()
for(i in hashes){
  
  tmp <- pull_tweets(i)
  
  storage[[i]] <- tmp
}

d <- rbindlist(storage, use.names = TRUE)

#----------------------- High level analysis -----------------------

#-------------------
# Sentiment analysis
#-------------------

lexicon <- get_sentiments(lexicon = "bing")

sent <- d %>%
  dplyr::select(c(status_id, text)) %>% 
  unnest_tokens(word, text)

sent_lex <- sent %>% 
  left_join(lexicon, by = c("word" = "word"))

# Aggregated sentiment - NOTE: no validation or topic modelling here, just raw lexicon counts

sent_lex %>% 
  filter(!is.na(sentiment)) %>% 
  group_by(sentiment) %>% 
  summarise(counter = n()) %>%
  ungroup() %>%
  mutate(props = counter / sum(counter))

#-------------------------
# Hashtag pairing analysis
#-------------------------

# Remove retweets

d1 <- d %>%
  filter(!is.na(flag)) # %>%
  #filter(isRetweet == "FALSE")

# Get date range of data to add to graph

dates <- gsub(" .*", "", d1$created)
earliest <- min(as.Date(d1$created, format = "%Y-%M-%D"))
latest <- min(as.Date(d1$created, format = "%Y-%M-%D"))

# Produce graph

CairoPNG("politics/output/ausday-tweet-combos.png", 800, 600)
d1 %>%
  group_by(flag) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  drop_na() %>%
  ggplot(aes(x = reorder(flag, counter), y = counter)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(title = "Frequency of hashtag combinations related to Australia Day",
       subtitle = str_wrap(paste0("Twitter data scraped on: ", Sys.Date(), ". Total tweets analysed: ", nrow(d1), ". ",
                                  "Earliest tweet in dataset: ", earliest, ". Most recent tweet in dataset: ", latest, ". ",
                                  "NOTE: Tweets with no explicit hashtags were filtered prior to calculating frequencies - this removed ", 
                                  (nrow(d)-nrow(d1)), " tweets."), 
                           width = 120),
       x = "Hashtag combination",
       y = "Frequency",
       caption = "Analysis: therunoffnews.com") +
  coord_flip() +
  theme_runoff(grids = TRUE) +
  theme(plot.title = element_text(face = "bold"))
dev.off()

#------------------
# Location analysis
#------------------

CairoPNG("politics/output/ausday-tweet-top-locations.png", 800, 600)
d1 %>%
  filter(nchar(as.character(location)) > 3) %>% # Remove nuisance blank space locations
  mutate(location = case_when(
         grepl("Melbourne", location) ~ "Melbourne, Victoria",
         grepl("Brisbane", location)  ~ "Brisbane, Queensland",
         grepl("Sydney", location)    ~ "Sydney, New South Wales",
         TRUE                         ~ location)) %>%
  group_by(location, flag) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  drop_na() %>%
  filter(location != "Palm Harbor, FL.") %>%
  top_n(counter, n = 20) %>%
  ggplot(aes(x = reorder(location, counter), y = counter)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(title = "Location of tweets using hashtags related to Australia Day",
       subtitle = str_wrap(paste0("Twitter data scraped on: ", Sys.Date(), ". Total tweets analysed: ", nrow(d1), ". ",
                                  "Earliest tweet in dataset: ", earliest, ". Most recent tweet in dataset: ", latest, ". ",
                                  "NOTE: Tweets with no explicit hashtags were filtered prior to calculating frequencies - this removed ", 
                                  (nrow(d)-nrow(d1)), " tweets. Locations that were NA were also filtered prior to analysis."), 
                           width = 120),
       x = "Location",
       y = "Frequency") +
  coord_flip() +
  theme_runoff(grids = TRUE) +
  theme(plot.title = element_text(face = "bold"))
dev.off()
