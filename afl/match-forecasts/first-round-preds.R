#------------------------------------------
# This script aims to extract probabilistic
# predictions for Round 1 of the 2021
# AFL season and return Win/Draw/Loss
# probabilities for each fixture
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 14 January 2021
#------------------------------------------

load("afl/data/first_round_team_priors.Rda")
load("afl/data/first_round_other_priors.Rda")

priors <- bind_rows(first_round_team_priors, first_round_other_priors)

#---------------------- Pre processing -----------------------------

data <- tmp



#---------------------- Run the model ------------------------------

# Put all input data into a list for Stan

N <- nrow()
K <- as.integer(length(unique()))
y <- as.numeric()
team <- as.integer()
ladder <- as.integer()
finals <- as.integer()
won_gf <- as.integer()
home_team <- as.integer()

N_pred <- nrow()
K_pred <- as.integer(length(unique()))
y_pred <- as.numeric()
team_pred <- as.integer()
ladder_pred <- as.integer()
finals_pred <- as.integer()
won_gf_pred <- as.integer()
home_team_pred <- as.integer()

stan_data <- list(N = N,
                  K = K,
                  y = y,
                  team = team,
                  ladder = ladder,
                  finals = finals,
                  won_gf = won_gf,
                  home_team = home_team,
                  N_pred = N_pred,
                  K_pred = K_pred,
                  y_pred = y_pred,
                  team_pred = team_pred,
                  ladder_pred = ladder_pred,
                  finals_pred = finals_pred,
                  won_gf_pred = won_gf_pred,
                  home_team_pred = home_team_pred)

# Run the Stan program

system.time({
  mod <- stan(data = stan_data, 
              file = "afl/first-round-pred.stan",
              iter = 2000, 
              chains = 3,
              seed = 123)
})

#---------------------- Extract results for each fixture -----------

# MAC HAS SOMETHING HERE!!!
