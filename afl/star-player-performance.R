#------------------------------------------
# This script aims to try and produce
# an analysis for star players of
# expectation to win and performance on 
# some key metrics
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 15 January 2021
#------------------------------------------

# Pull data back to 2017

years <- c(seq(from = 2017, to = 2020, by = 1))
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

# List of star players

stars <- c("Dustin Martin", "Patrick Dangerfield", "Lachie Neale", "Nat Fyfe")

the_finals <- c("EF", "SF", "QF", "PF", "GF") # Removes finals as these might bias analysis

#--------------------------
# DATASET 1: PLAYER METRICS
#--------------------------

player_metrics <- all_seasons %>%
  filter(round %ni% the_finals) %>%
  mutate(player_name = paste0(first_name, " ", surname)) %>%
  filter(player_name %in% stars) %>%
  group_by(season, round, player_name, playing_for) %>%
  summarise(kicks = sum(kicks),
            marks = sum(marks),
            contested_marks = sum(contested_marks),
            contested_possessions = sum(contested_possessions),
            handballs = sum(handballs)) %>%
  ungroup() %>%
  gather(key = metric, value = value, c(kicks, marks, contested_marks, 
                                      contested_possessions, handballs))

#-------------------------
# DATASET 2: TEAM POSITION
#-------------------------

pos_init <- all_seasons %>%
  filter(round %ni% c("EF", "GF", "PF", "QF", "SF")) %>% # Remove finals
  group_by(season, round, home_team, away_team) %>%
  summarise(home_score = mean(home_score),
            away_score = mean(away_score)) %>%
  ungroup() %>%
  mutate(outcome = case_when(
    home_score > away_score  ~ "Home Win",
    home_score < away_score  ~ "Away Win",
    home_score == away_score ~ "Draw")) %>%
  mutate(round = as.numeric(round)) %>%
  arrange(season, round)

# Loop over each team to summarise matches played and get points at each round

teams <- unique(pos_init$home_team)
rounds <- unique(pos_init$round)
rounds <- rounds[!rounds %in% 1] # Remove Round 1 as there is no prior round

compute_points <- function(team, season){
  
  ladder_list <- list()
  
  for(r in rounds){
    
    # Sum points for all games prior to the current round
    
    tmp_home <- pos_init %>%
      filter(season == season) %>%
      filter(round < r) %>%
      filter(home_team == team) %>%
      mutate(outcome = case_when(
        outcome == "Home Win" ~ "Win",
        outcome == "Away Win" ~ "Loss",
        outcome == "Draw"     ~ "Draw"))
    
    tmp_away <- pos_init %>%
      filter(season == season) %>%
      filter(round < r) %>%
      filter(away_team == team) %>%
      mutate(outcome = case_when(
        outcome == "Home Win" ~ "Loss",
        outcome == "Away Win" ~ "Win",
        outcome == "Draw"     ~ "Draw"))
    
    tmp_all <- bind_rows(tmp_home, tmp_away) %>%
      mutate(team = team) %>%
      group_by(team, season, outcome) %>%
      summarise(counter = n()) %>%
      ungroup() %>%
      mutate(pts = case_when(
        outcome == "Win"  ~ 4*counter,
        outcome == "Draw" ~ 2*counter,
        outcome == "Loss" ~ 0)) %>%
      group_by(team, season) %>%
      summarise(pts = sum(pts)) %>%
      ungroup() %>%
      mutate(round = r)
    
    ladder_list[[r]] <- tmp_all
  }
  # Bind all together and rank ladder based on points for each season
  team_position <- rbindlist(ladder_list, use.names = TRUE)
}

# 2017

output_2017 <- list()
for(t in teams){
  tmp <- compute_points(team = t, season = 2017)
  output_2017[[t]] <- tmp
}
output_2017 <- rbindlist(output_2017, use.names = TRUE)

# 2018

output_2018 <- list()
for(t in teams){
  tmp <- compute_points(team = t, season = 2018)
  output_2018[[t]] <- tmp
}
output_2018 <- rbindlist(output_2018, use.names = TRUE)

# 2019

output_2019 <- list()
for(t in teams){
  tmp <- compute_points(team = t, season = 2019)
  output_2019[[t]] <- tmp
}
output_2019 <- rbindlist(output_2019, use.names = TRUE)

# 2020

output_2020 <- list()
for(t in teams){
  tmp <- compute_points(team = t, season = 2020)
  output_2020[[t]] <- tmp
}
output_2020 <- rbindlist(output_2020, use.names = TRUE)

# All

pts <- bind_rows(output_2017, output_2018, output_2019, output_2020)

#--------------------------
# Merge both files together
#--------------------------

full <- player_metrics %>%
  mutate(round = as.numeric(round)) %>%
  inner_join(pts, by = c("playing_for" = "team", "season" = "season",
                                  "round" = "round"))

player_games <- all_seasons %>%
  filter(round %ni% c("EF", "GF", "PF", "QF", "SF")) %>%
  mutate(round = as.numeric(round)) %>%
  filter(round %ni% the_finals) %>%
  mutate(player_name = paste0(first_name, " ", surname)) %>%
  filter(player_name %in% stars) %>%
  dplyr::select(c(season, round, player_name, playing_for, home_team, away_team))

team_position_opp <- pts %>%
  rename(opp_pts = pts)

full_setup <- full %>%
  inner_join(player_games, by = c("season" = "season", "round" = "round", 
                                 "player_name" = "player_name",
                                 "playing_for" = "playing_for")) %>%
  mutate(opponent = case_when(
         home_team == playing_for ~ away_team,
         home_team != playing_for ~ home_team)) %>%
  dplyr::select(c(player_name, season, round, playing_for, metric, value, opponent, pts)) %>%
  inner_join(team_position_opp, by = c("season" = "season", "round" = "round",
                                       "opponent" = "team"))

# Compute differential

full_diff <- full_setup %>%
  mutate(differential = pts - opp_pts) %>%
  group_by(season, round, player_name, playing_for, metric) %>%
  summarise(value = mean(value),
            differential = mean(differential)) %>%
  ungroup()

#---------------------- Analysis -----------------------------------

full_diff %>%
  filter(metric %ni% c("marks", "contested_marks")) %>%
  mutate(metric = str_to_title(metric),
         metric = gsub("_", " ", metric)) %>%
  ggplot(aes(x = differential, y = value)) +
  geom_point(colour = "#A09BE7", alpha = 0.8) +
  geom_smooth(formula = y ~ x, method = "lm", colour = "#2274A5") +
  labs(title = "Relationship between star player performance and expectation of win",
       subtitle = "Each point represents a match. Data from 2017-2020 seasons.",
       x = "Difference in points between star's team and opponent's",
       y = "Match sum") +
  facet_grid(rows = vars(metric), cols = vars(player_name)) +
  theme_runoff(grids = TRUE) +
  theme(strip.background = element_rect(colour = "#2274A5", fill = "#2274A5"),
        strip.text = element_text(colour = "white"),
        panel.background = ggplot2::element_rect(fill = "#E1E6E6", colour = "#E1E6E6"),
        plot.background = ggplot2::element_rect(fill = "#E1E6E6", colour = "#E1E6E6"),
        legend.background = ggplot2::element_rect(fill = "#E1E6E6", colour = "#E1E6E6"),
        legend.box.background = ggplot2::element_rect(fill = "#E1E6E6", colour = "#E1E6E6"),
        legend.key = ggplot2::element_rect(fill = "#E1E6E6", colour = "#E1E6E6"))
