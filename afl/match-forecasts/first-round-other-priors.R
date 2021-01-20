#------------------------------------------
# This script aims to fit basic statistical
# models to obtain priors to use in
# Bayesian predictive models
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 14 January 2021
#------------------------------------------

# Pull data all the way back to 2001

years <- c(seq(from = 2001, to = 2020, by = 1))
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

ladder_1 <- function(){
  
  ladders <- data_2 %>%
    filter(season != 2020) %>%
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
    mutate(ladder_pos = dense_rank(-pts)) %>%
    ungroup()
}

lads <- ladder_1()

# Main function

lag_ladder <- function(team){
  
  # Wrangle data in lag format
  
  seasons <- unique(lads$season)
  calc_list <- list()
    
  for(s in seasons){
      
    s_plus <- s+1
    
    first_round_home <- data_2 %>%
      filter(season == s_plus) %>%
      filter(round == "1") %>%
      filter(home_team == team)
    
    first_round_away <- data_2 %>%
      filter(season == s_plus) %>%
      filter(round == "1") %>%
      filter(away_team == team)
    
    if(nrow(first_round_home) > 0){
      first_round <- first_round_home %>%
        mutate(outcome = case_when(
          home_team == team & outcome == "Home Win" ~ "Win",
          home_team == team & outcome == "Away Win" ~ "Loss",
          outcome == "Draw"                         ~ "Draw")) %>%
        dplyr::select(c(outcome))
    } else{
      first_round <- first_round_away %>%
        mutate(outcome = case_when(
          outcome == "Draw"                         ~ "Draw",
          away_team == team & outcome == "Home Win" ~ "Loss",
          away_team == team & outcome == "Away Win" ~ "Win")) %>%
        dplyr::select(c(outcome))
    }
      
    prior_season <- lads %>%
      filter(team == team) %>%
      filter(season == s) %>%
      dplyr::select(c(team, season, ladder_pos))
        
    joined <- cbind(prior_season, first_round)
    
    calc_list[[s]] <- joined
  }
  
  ladder_final <- rbindlist(calc_list, use.names = TRUE)
  return(ladder_final)
}

# Run the lag function

lag_output <- list()
for(t in teams){
  tmp <- lag_ladder(team = t)
  lag_output[[t]] <- tmp
}
lag_final <- rbindlist(lag_output, use.names = TRUE)

# Finals

finals <- data_2

# Won GF

won_gf <- data_2

# Home Team

home_team <- data_2 %>%
  filter(season != 2020) %>%
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
