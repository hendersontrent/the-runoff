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

#----------------
# DATASET 1: TEAM
#----------------

# Get just most recent 3 seasons

data_1 <- all_seasons %>%
    filter(season >= 2018)

#---------------------------
# DATASET 2: OTHER VARIABLES
#---------------------------

# Need to filter out 2020 as it was an anomalous season (and had no proper home teams)
# as we just want to fit a single statistical model with all these variables and not
# one by one

data_2 <- all_seasons %>%
    filter(season < 2020)

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

# Run a model

mod <- something(y ~ x, data = tmp)

#-------------------------
# MODEL 2: OTHER VARIABLES
#-------------------------



#---------------------- Extract priors and save --------------------

# Extraction (NEED 1 ROW PER VARIABLE!!!)

team_priors <- mod %>%
    group_by() %>% 
    summarise() %>% 
    ungroup() %>% 
    dplyr::select(c(coefficient, mu, sd))

other_priors <- mod %>%
    group_by() %>% 
    summarise() %>% 
    ungroup() %>% 
    dplyr::select(c(coefficient, mu, sd))

# Bind together

first_round_priors <- bind_rows(team_priors, other_priors)

# Save for import and use in predictive model

save(first_round_priors, file = "afl/data/first_round_priors.Rda")
