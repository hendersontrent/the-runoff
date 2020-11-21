#------------------------------------------
# This script sets out to pull the data
# needed for a classification algorithm and
# prep it for modelling
#
# NOTE: This script requires setup.R to
# have been run first
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

winner_loser_data <- tmp1 %>%
  mutate(winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    TRUE                    ~ "Remove")) %>%
  filter(winner != "Remove") %>%
  mutate(did_i_win = case_when(
    playing_for == winner ~ "Win",
    TRUE                  ~ "Lose")) %>%
  mutate(did_i_win = as.factor(did_i_win)) %>%
  group_by(season, round, did_i_win) %>%
  summarise(kicks = sum(kicks),
            marks = sum(marks),
            handballs = sum(handballs),
            hit_outs = sum(hit_outs),
            tackles = sum(tackles),
            rebounds = sum(rebounds),
            inside_50s = sum(inside_50s),
            clearances = sum(clearances),
            clangers = sum(clangers),
            frees_for = sum(frees_for),
            frees_against = sum(frees_against),
            contested_possessions = sum(contested_possessions),
            uncontested_possessions = sum(uncontested_possessions),
            contested_marks = sum(contested_marks),
            marks_inside_50 = sum(marks_inside_50),
            bounces = sum(bounces),
            goal_assists = sum(goal_assists)) %>%
  ungroup() %>%
  dplyr::select(-c(season, round))

#---------------------- Save data for use --------------------------

write_csv(winner_loser_data, file = "afl/data/winner_loser_data.csv")
