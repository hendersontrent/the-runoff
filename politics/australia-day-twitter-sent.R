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
           grepl("#AustraliaDay", text)                               ~ "#AustraliaDay only",
           grepl("#InvasionDay", text)                                ~ "#InvasionDay only",
           grepl("#AustraliaDay", text) & grepl("#InvasionDay", text) ~ "Both #AustraliaDay and #InvasionDay"))
  
  return(df)
}

ausday <- pull_tweets("#AustraliaDay")
invday <- pull_tweets("#InvasionDay")

d <- bind_rows(ausday, invday)

#----------------------- High level analysis -----------------------

#


