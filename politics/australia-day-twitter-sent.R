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

tweets <- searchTwitter("#AustraliaDay", n = 3200, lang = "en")
tweets_df <- twListToDF(tweets) # Convert to data frame

#----------------------- High level analysis -----------------------


