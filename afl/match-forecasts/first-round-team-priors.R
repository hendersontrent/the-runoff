#------------------------------------------
# This script aims to fit basic statistical
# models to obtain priors to use in
# Bayesian predictive models
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 14 January 2021
#------------------------------------------

# Pull data back to 2018

years <- c(seq(from = 2018, to = 2020, by = 1))
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
# DATASET 1: TEAM
#----------------

# Get just most recent 3 seasons and summarise data for each team

data_1 <- all_seasons %>%
    filter(season >= 2018) %>%
    group_by(season, round, home_team, away_team) %>%
    summarise(home_score = mean(home_score),
              away_score = mean(away_score)) %>%
    ungroup() %>%
    filter(round %ni% c("EF", "GF", "PF", "QF", "SF")) %>% # Remove finals
    mutate(outcome = case_when(
           home_score > away_score  ~ "Home Win",
           home_score < away_score  ~ "Away Win",
           home_score == away_score ~ "Draw"))

#---------------------- Fit statistical models ---------------------

# Need to get priors for the following variables:
#   - team
#   - ladder
#   - finals
#   - won_gf
#   - home_team

#--------------
# MODEL 1: TEAM
#--------------

# Run a model looping over each team

teams <- unique(data_1$home_team)
mod_1 <- list()

for(i in team){
    
    # Filter data
    
    tmp_home <- data_1 %>%
        filter(home_team == i)
    
    tmp_away <- data_1 %>%
        filter(away_team == i)
    
    tmp <- bind_rows(tmp_home, tmp_away)
    
    # Fit model
    
    m <- something(outcome ~ team, 
                   data = data_1)
    
    # Extract outputs
    
    mod_1[[i]] <- m
}

mod_1_outs <- rbindlist(mod_1, use.names = TRUE)

#---------------------- Extract priors and save --------------------

# Extraction (NEED 1 ROW PER VARIABLE!!!)

first_round_team_priors <- mod_1_outs %>%
    group_by() %>% 
    summarise() %>% 
    ungroup() %>% 
    dplyr::select(c(coefficient, mu, sd))

# Save for import and use in predictive model

save(first_round_team_priors, file = "afl/data/first_round_team_priors.Rda")
