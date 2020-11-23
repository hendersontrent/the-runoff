#------------------------------------------
# This script sets out to pull data and
# prepare it for forecasts of the first 
# round for the 2021 AFL season
#
# NOTE: This script requires setup.R to
# have been run first
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 23 November 2020
#------------------------------------------

#' Define a reusable function
#' @param start_date the start date of the AFL season/year to get data for
#' @param end_date the end date of the AFL season/year to get data for
#' 

pull_afl_data <- function(start_date, end_date){
  tmp <- get_afltables_stats(start_date = start_date, end_date = end_date) %>%
    clean_names() %>%
    mutate(season = gsub("-.*", "\\1", date),
           season = as.numeric(season))
  
  if(nrow(tmp) == 0){
    print("Data not pulled successfully.")
  } else{
    return(tmp)
  }
}

# Pull data for the last 10 seasons

season_2011 <- pull_afl_data(start_date = "2011-01-01", end_date = "2011-12-01")
season_2012 <- pull_afl_data(start_date = "2012-01-01", end_date = "2012-12-01")
season_2013 <- pull_afl_data(start_date = "2013-01-01", end_date = "2013-12-01")
season_2014 <- pull_afl_data(start_date = "2014-01-01", end_date = "2014-12-01")
season_2015 <- pull_afl_data(start_date = "2015-01-01", end_date = "2015-12-01")
season_2016 <- pull_afl_data(start_date = "2016-01-01", end_date = "2016-12-01")
season_2017 <- pull_afl_data(start_date = "2017-01-01", end_date = "2017-12-01")
season_2018 <- pull_afl_data(start_date = "2018-01-01", end_date = "2018-12-01")
season_2019 <- pull_afl_data(start_date = "2019-01-01", end_date = "2019-12-01")
season_2020 <- pull_afl_data(start_date = "2020-01-01", end_date = "2020-12-01")

#---------------------- Pre processing -----------------------------

#----------------
# Merge and clean
#----------------

# Merge datasets

tmp1 <- bind_rows(season_2011, season_2012, season_2013, season_2014, season_2015,
                  season_2016, season_2017, season_2018, season_2019, season_2020)

# Compute winner binary variable and retain only variables of interest
# by summing over each game. Removes goals and behinds as this will obviously
# be associated with the game outcome

the_finals <- c("EF", "SF", "QF", "PF", "GF") # Removes finals

tmp2 <- tmp1 %>%
  filter(round %ni% the_finals)

#---------------------- Get data in aggregated form ----------------

#------------------
# Grand Finalists
#------------------



#-------------------------------
# General home team probability
#-------------------------------

# Get probability of winning at home for each team

extract_home_probs <- function(){
  
  # Aggregate data
  
  tmp3 <- tmp2 %>%
    filter(season >= 2015) # Just last 5 seasons ignoring 2020 due to COVID
  
  # Compute each team and bind together
  
  all_teams <- unique(tmp2$home_team)
  empty_list <- list()
  
  for(a in all_teams){
    tmp4 <- tmp3 %>%
      filter(home_team == a) %>%
      dplyr::select(c(season, round, home_score, away_score)) %>%
      distinct() %>%
      mutate(winner = case_when(
        home_score > away_score ~ "Home Win",
        away_score > home_score ~ "Away Win")) %>%
      group_by(season, winner) %>%
      summarise(counter = n()) %>%
      group_by(season) %>%
      mutate(props = round((counter / sum(counter))*100, digits = 2)) %>%
      ungroup() %>%
      filter(winner == "Home Win") %>%
      mutate(home_team = a) %>%
      group_by(home_team) %>%
      summarise(avg = mean(props),
                sd = sd(props)) %>%
      ungroup()
    
    empty_list[[a]] <- tmp4
  }
  
  home_probs <- rbindlist(empty_list, use.names = TRUE)
  
  # Return summarised data
  
  return(home_probs)
}

home_probs <- extract_home_probs()

# Sense check plot

CairoPNG("afl/output/home-team-win-probs.png", 800, 600)
home_probs %>%
  ggplot() +
  geom_point(aes(x = reorder(home_team, avg), y = avg), stat = "identity", size = 5) +
  geom_segment(aes(x = home_team, xend = home_team, y = (avg + 2*sd), yend = (avg - 2*sd)), alpha = 0.5, size = 3) +
  labs(title = "Home team win probability over the 2015-2019 seasons",
       subtitle = "Shaded bars indicate +- 2 SD regions.",
       x = "Team",
       y = "Home Team Win Probability") +
  scale_y_continuous(labels = function(x)paste0(x,"%")) +
  coord_flip() +
  theme_runoff()
dev.off()

#------------------
#
#------------------



#------------------
#
#------------------


