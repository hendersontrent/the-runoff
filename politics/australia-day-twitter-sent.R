#--------------------------------------------
# This script sets out to scrape Twitter
# data related to the #AustraliaDay tag
#--------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 19 January 2021
#------------------------------------------

library(rtweet)
library(twitteR)

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
  
  if(!grepl("#", hashtag)){
    stop("hashtag should have a # at the front and be written as one word.")
  }
  
  tweets <- searchTwitter(hashtag, n = 3200, lang = "en")
  
  # Convert to dataframe and add some useful flags as columns
  
  df <- twListToDF(tweets) %>%
    mutate(hashtag = hashtag) %>%
    mutate(flag = case_when(
           grepl("#AustraliaDay", text) & !grepl("#InvasionDay", text) & !grepl("#changethedate", text) ~ "#AustraliaDay only",
           grepl("#InvasionDay", text) & !grepl("#AustraliaDay", text) & !grepl("#changethedate", text) ~ "#InvasionDay only",
           grepl("#changethedate", text) & !grepl("#AustraliaDay", text) & !grepl("#InvasionDay", text) ~ "#changethedate only",
           grepl("#AustraliaDay", text) & grepl("#InvasionDay", text) & !grepl("#changethedate", text)  ~ "#AustraliaDay and #InvasionDay",
           grepl("#AustraliaDay", text) & !grepl("#InvasionDay", text) & grepl("#changethedate", text)  ~ "#AustraliaDay and #changethedate",
           !grepl("#AustraliaDay", text) & grepl("#InvasionDay", text) & grepl("#changethedate", text)  ~ "#InvasionDay and #changethedate",
           grepl("#AustraliaDay", text) & grepl("#InvasionDay", text) & grepl("#changethedate", text)   ~ "#AustraliaDay, #InvasionDay, and #changethedate"))
  
  return(df)
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

#


