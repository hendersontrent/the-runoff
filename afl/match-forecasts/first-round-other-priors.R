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

ladder <- data_2

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
