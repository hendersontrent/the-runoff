#------------------------------------------
# This script sets out to pull the data
# needed for a classification algorithm and
# prep it for modelling
#
# NOTE: This script requires setup.R to
# have been run first
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 21 November 2020
#------------------------------------------

# Pull data all the way back to 2011

years <- c(seq(from = 2011, to = 2020, by = 1))
store <- list()

for(i in years){
  
  start_date <- as.character(paste0(i,"-01-01"))
  end_date <- as.character(paste0(i,"-12-01"))
  
  tmp <- get_afltables_stats(start_date = start_date, end_date = end_date) %>%
    clean_names() %>%
    mutate(season = gsub("-.*", "\\1", date),
           season = as.numeric(season))
  
  store[[i]] <- tmp
}

all_seasons <- rbindlist(store, use.names = TRUE)

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

the_finals <- c("EF", "SF", "QF", "PF", "GF") # Removes finals as these might bias analysis

winner_loser_data <- tmp1 %>%
  filter(round %ni% the_finals) %>%
  mutate(winner = case_when(
    home_score > away_score ~ home_team,
    away_score > home_score ~ away_team,
    TRUE                    ~ "Remove")) %>%
  filter(winner != "Remove") %>%
  mutate(did_i_win = case_when(
    playing_for == winner ~ 1,
    TRUE                  ~ 0)) %>%
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
            bounces = sum(bounces)) %>%
  ungroup() %>%
  dplyr::select(-c(season, round))

#---------------------- Save data for use --------------------------

write_csv(winner_loser_data, "afl/data/winner_loser_data.csv")
