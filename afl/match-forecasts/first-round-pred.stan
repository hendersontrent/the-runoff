//
// This Stan program aims to predict the winner of all
// first round AFL games for the 2021 season. The program
// uses informative priors of historical data to predict
// each individual game.
//
// The variables included were decided upon for the purpose 
// of modelling and may need to be updated or changed.
//
// NOTE: Prior for team was taken from only most recent three
// seasons' data due to player changes. Priors for the remaining 
// variables were obtained on all historical data:
//     1. ladder
//     2. finals
//     3. won_gf
//     4. home_team
//

//
// Author: Trent Henderson, 14 January 2021
//

data {

  // Base modelling data

  int<lower=3> K; // Number of outcome categories - Win/Loss/Draw
  int<lower=0> N; // Sample size
  int<lower=1,upper=K> y[N]; // Outcome category
  int<lower=1,upper=18> team[N]; // Team
  int<lower=1,upper=18> ladder[N]; // Last season ladder position
  int<lower=1,upper=2> finals[N]; // Made finals - Yes/No
  int<lower=1,upper=2> won_gf[N]; // Won Grand Final - Yes/No
  int<lower=1,upper=2> home_team[N]; // Home team

  // Prediction modelling data (Round 1 matchups of 2021 Season)

  int<lower=0> N_pred; // Sample size of data to predict based on
  int<lower=1,upper=K> y_pred[N]; // Outcome category
  int<lower=1,upper=18> team_pred[N]; // Team
  int<lower=1,upper=18> ladder_pred[N]; // Last season ladder position
  int<lower=1,upper=2> finals_pred[N]; // Made finals - Yes/No
  int<lower=1,upper=2> won_gf_pred[N]; // Won Grand Final - Yes/No
  int<lower=1,upper=2> home_team_pred[N]; // Home team

  // Priors

  real team_mu[N]; // Mean for coefficient of each team
  real team_sd[N]; // SD for coefficient of each team
  real ladder_mu[N]; // Mean for coefficient of ladder position in previous season
  real ladder_sd[N]; // SD for coefficient of ladder position in previous season
  real finals_mu[N]; // Mean for coefficient of whether finals were made in previous season
  real finals_sd[N]; // SD for coefficient of whether finals were made in previous season
  real won_gf_mu[N]; // Mean for coefficient of whether team won Grand Final or not
  real won_gf_sd[N]; // SD for coefficient of whether team won Grand Final or not
  real home_team_mu[N]; // Mean for coefficient of home team advantage
  real home_team_sd[N]; // SD for coefficient of home team advantage

}

parameters {
    
    // Intercept
  
    real alpha; // Intercept variable
    real<lower=0> sigma; // Standard deviation for the intercept

    // Coefficients

    real beta_team
    real beta_ladder
    real beta_finals
    real beta_won_gf
    real beta_home_team
    
    // Number of response variable categories to ensure model doesn't fit beyond them
  
    ordered[K-1] c;
}

transformed parameters {
    
    // Variable for likelihood function

    vector[N] eta;

    for (n in 1:N){
      eta[n] = alpha + team[n] * beta_team + ladder[n] * beta_ladder + finals[n] * beta_finals + won_gf[n] * beta_won_gf + home_team[n] * beta_home_team;
    }
}

model {
    
    // Priors - NOTE: These may not actually be normal, need to cross-check and change distributions if necessary

    alpha ~ normal(0,sigma);
    beta_team ~ normal(team_mu, team_sd);
    beta_ladder ~ normal(ladder_mu, ladder_sd);
    beta_finals ~ normal(finals_mu, finals_sd);
    beta_won_gf ~ normal(won_gf_mu, won_gf_sd);
    beta_home_team ~ normal(home_team_mu, home_team_sd);

    // Model
  
    for (n in 1:N){
        y[n] ~ ordered_logistic(eta[n], c);
    }
}

generated quantities {

    // Instantiate some variables for calculation of log-likelihood and posterior predictive checks

    vector[N] log_lik; // Pointwise log-likelihood for leave-one-out cross validation
    vector[N] y_rep; // Replications from posterior predictive distribution

    for (n in 1:N){
        log_lik[n] = ordered_logistic_lpmf(y[n] | eta[n], sigma); // CHECK AGAINST MODEL ON MAC!!!
        y_rep[n] = ordered_logistic_rng(eta[n], sigma);
    }

    // Predictions for Round 1 of the 2021 season

    for (n in 1:N_pred){
        y_pred[n] ~ ordered_logistic_rng(eta, c); // FIX ETA HERE!!!
    }
}