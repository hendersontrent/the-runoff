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

# Loop over each team to summarise matches played in a consistent 1 column way

teams <- unique(data_1$home_team)
matches <- list()

for(i in teams){
    
    # Filter data
    
    tmp_home <- data_1 %>%
        filter(home_team == i) %>%
        mutate(outcome = case_when(
               outcome == "Home Win" ~ "Win",
               outcome == "Away Win" ~ "Loss",
               outcome == "Draw"     ~ "Draw")) %>%
        mutate(team = i) %>%
        dplyr::select(season, round, team, outcome)
    
    tmp_away <- data_1 %>%
        filter(away_team == i) %>%
        mutate(outcome = case_when(
            outcome == "Home Win" ~ "Loss",
            outcome == "Away Win" ~ "Win",
            outcome == "Draw"     ~ "Draw")) %>%
        mutate(team = i) %>%
        dplyr::select(season, round, team, outcome)
    
    tmp_all <- bind_rows(tmp_home, tmp_away)
    
    matches[[i]] <- tmp_all
}

match_sum <- rbindlist(matches, use.names = TRUE) %>%
    mutate(outcome = factor(outcome, levels = c("Loss", "Draw", "Win")))

#---------------------- Fit statistical models ---------------------

#--------------
# MODEL 1: TEAM
#--------------

# Run a basic model

m <- MASS::polr(outcome ~ 1 + team, data = match_sum, Hess = TRUE)

#---------------------- Extract priors and save --------------------

mod_outs <- as.data.frame(summary(m)$coefficients)

# Check distribution

mod_outs %>%
    ggplot(aes(x = Value)) +
    geom_density(alpha = 0.4, colour = "black") +
    labs(x = "Coefficient",
         y = "Density") +
    scale_x_continuous(limits = c(-2,2.5)) +
    theme_runoff(grids = TRUE)

# Calculate mean and SD of coefficients

first_round_team_priors <- mod_outs %>%
    summarise(mu = mean(Value),
              sd = sd(Value)) %>% 
    ungroup() %>% 
    mutate(coefficient = "team") %>%
    dplyr::select(c(coefficient, mu, sd))

# Save for import and use in predictive model

save(first_round_team_priors, file = "afl/data/first_round_team_priors.Rda")
