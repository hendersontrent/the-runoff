#------------------------------------------
# This script aims to fit basic statistical
# models to obtain priors to use in
# Bayesian predictive models
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 14 January 2021
#------------------------------------------

# Pull data all the way back to 2001

years <- c(seq(from = 2001, to = 2019, by = 1))
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

#---------------------------
# DATASET 2: OTHER VARIABLES
#---------------------------

# High level aggregation

data_2 <- all_seasons %>%
  group_by(season, round, home_team, away_team) %>%
  summarise(home_score = mean(home_score),
            away_score = mean(away_score)) %>%
  ungroup() %>%
  mutate(outcome = case_when(
    home_score > away_score  ~ "Home Win",
    home_score < away_score  ~ "Away Win",
    home_score == away_score ~ "Draw"))

# Ladder

teams <- unique(data_2$home_team)
ladder_list <- list()

prep_ladder <- function(){
  
  ladders <- data_2 %>%
    filter(round %ni% c("EF", "GF", "PF", "QF", "SF")) # Remove finals
  
  for(i in teams){
    
    tmp_home <- ladders %>%
      filter(home_team == i) %>%
      mutate(outcome = case_when(
             outcome == "Home Win" ~ "Win",
             outcome == "Away Win" ~ "Loss",
             outcome == "Draw"     ~ "Draw"))
    
    tmp_away <- ladders %>%
      filter(away_team == i) %>%
      mutate(outcome = case_when(
        outcome == "Home Win" ~ "Loss",
        outcome == "Away Win" ~ "Win",
        outcome == "Draw"     ~ "Draw"))
    
    tmp_all <- bind_rows(tmp_home, tmp_away) %>%
      mutate(team = i) %>%
      group_by(team, season, outcome) %>%
      summarise(counter = n()) %>%
      ungroup() %>%
      mutate(pts = case_when(
             outcome == "Win"  ~ 4*counter,
             outcome == "Draw" ~ 2*counter,
             outcome == "Loss" ~ 0)) %>%
      group_by(team, season) %>%
      summarise(pts = sum(pts)) %>%
      ungroup()
    
    ladder_list[[i]] <- tmp_all
  }
  
  # Bind all together and rank ladder based on points for each season
  
  ladder_all_teams <- rbindlist(ladder_list, use.names = TRUE)  %>%
    group_by(season) %>%
    mutate(ladder_pos = dense_rank(pts)) %>%
    ungroup()
  
  first_rounds <- data_2 %>%
    filter(round == "1")
  
  return(ladder_final)
}

# Finals

finals <- data_2

# Won GF

won_gf <- data_2

# Home Team

home_team <- data_2 %>%
  group_by(season, outcome) %>%
  summarise(counter = n()) %>%
  group_by(season) %>%
  mutate(probs = counter / sum(counter)) %>%
  ungroup() %>%
  filter(outcome == "Home Win") %>%
  mutate(coefficient = "home_team") %>%
  group_by(coefficient) %>%
  summarise(mu = mean(probs),
            sd = sd(probs)) %>%
  ungroup()

#---------------------- Fit statistical models ---------------------

#-------
# ladder
#-------



#-------
# finals
#-------



#-------
# won_gf
#-------



#---------------------- Extract priors and save --------------------

mod_outs <- as.data.frame(summary(m)$coefficients)

# Check distribution

mod_outs %>%
  ggplot(aes(x = Value)) +
  geom_density(alpha = 0.4, colour = "black") +
  labs(x = "Coefficient",
       y = "Density") +
  scale_x_continuous(limits = c(-2,2.5)) +
  theme_runoff(grids = TRUE) +
  facet_wrap(~coefficient)

# Calculate mean and SD of coefficients

first_round_other_priors <- mod_outs %>%
  summarise(mu = mean(Value),
            sd = sd(Value)) %>% 
  ungroup() %>% 
  mutate(coefficient = "team") %>%
  dplyr::select(c(coefficient, mu, sd))

# Save for import and use in predictive model

save(first_round_other_priors, file = "afl/data/first_round_other_priors.Rda")
