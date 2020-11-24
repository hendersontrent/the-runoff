#------------------------------------------
# This script sets out to produce run a
# Latent Profile Analysis of players across
# the last 5 seasons
#
# NOTE: This script requires setup.R to
# have been run first
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 24 November 2020
#------------------------------------------

#' Define a reusable function
#' @param start_date the start date of the AFL season/year to get data for
#' @param end_date the end date of the AFL season/year to get data for
#' 

pull_afl_data <- function(start_date, end_date){
  tmp <- get_afltables_stats(start_date = start_date, end_date = end_date) %>%
    clean_names() %>%
    mutate(season = gsub("-.*", "\\1", date),
           season = as.numeric(season))
  
  if(nrow(tmp) == 0){
    print("Data not pulled successfully.")
  } else{
    return(tmp)
  }
}

# Pull data for last 5 seasons

season_2016 <- pull_afl_data(start_date = "2016-01-01", end_date = "2016-12-01")
season_2017 <- pull_afl_data(start_date = "2017-01-01", end_date = "2017-12-01")
season_2018 <- pull_afl_data(start_date = "2018-01-01", end_date = "2018-12-01")
season_2019 <- pull_afl_data(start_date = "2019-01-01", end_date = "2019-12-01")
season_2020 <- pull_afl_data(start_date = "2020-01-01", end_date = "2020-12-01")

#----------------------- Pre processing ----------------------------

#----------------
# Merge and clean
#----------------

tmp1 <- bind_rows(season_2016, season_2017, season_2018, season_2019, season_2020)

the_finals <- c("EF", "SF", "QF", "PF", "GF") # Removes finals as these might bias analysis

# Get averages for each player

player_data <- tmp1 %>%
  filter(round %ni% the_finals) %>%
  mutate(player_name = paste0(first_name, " ", surname)) %>%
  group_by(player_name, playing_for) %>%
  summarise(kicks = mean(kicks),
            marks = mean(marks),
            goals = mean(goals),
            behinds = mean(behinds),
            contested_possessions = mean(contested_possessions),
            contested_marks = mean(contested_marks),
            inside_50s = mean(inside_50s),
            clearances = mean(clearances),
            handballs = mean(handballs)) %>%
  ungroup()

player_data <- mutate(player_data, id = rownames(player_data)) # Helps with LPA merge

#----------------------- Run the LPA -------------------------------

m1 <- player_data %>%
  dplyr::select(-c(id, player_name, playing_for)) %>%
  single_imputation() %>%
  estimate_profiles(1:6,
                    variances = c("equal", "varying"),
                    covariances = c("zero", "varying"))

#----------------------- Assess the fit indices --------------------

m1 %>%
  compare_solutions(statistics = c("AIC", "BIC", "AWE", "CLC", "KIC"))

#----------------------- Pull outputs into a dataframe -------------

# Get outputs in a dataframe

lpa_outputs <- get_data(m1)

# Filter to just Model 6/Classes 5 as this model had the best analytic hierarchy

lpa_outputs_filt <- lpa_outputs %>%
  filter(model_number == "6" & classes_number == 5)

# Join back in to main dataset and retain just the class for each player
# that has the highest probability

final_profiles <- player_data %>%
  mutate(id = as.integer(id)) %>%
  inner_join(lpa_outputs_filt, by = c("id" = "id", "kicks" = "kicks", "marks" = "marks",
                                      "goals" = "goals", "behinds" = "behinds",
                                      "contested_possessions" = "contested_possessions", 
                                      "contested_marks" = "contested_marks",
                                      "inside_50s" = "inside_50s", 
                                      "clearances" = "clearances",
                                      "handballs" = "handballs")) %>%
  group_by(id) %>%
  slice(which.max(Probability)) %>%
  ungroup() %>%
  mutate(Class = as.factor(Class)) %>%
  dplyr::select(-c(id, model_number, classes_number, Class_prob, Probability))

#----------------------- Data visualisation ------------------------

# Requires nodes and edges

nodes <- final_profiles %>%
  dplyr::select(c(player_name, Class)) %>%
  rename(id = player_name,
         group = Class) %>% # Renames into variables required for JavaScript library
  mutate(group = as.factor(group)) %>%
  mutate(id = make.unique(id, sep = "_")) # Some players have the same name

edges <- expand.grid(from = nodes$id, to = nodes$id) %>%
  filter(from != to)

sample_size <- floor(0.0005 * nrow(edges))
set.seed(123)
train_ind <- sample(seq_len(nrow(edges)), size = sample_size)
train <- edges[train_ind,]

#-------------------
# Make network graph
#-------------------

vis.nodes <- nodes
vis.nodes$shadow <- TRUE # Nodes will drop shadow
vis.nodes$title  <- vis.nodes$id # Text on click

visNetwork(vis.nodes, train, width = "100%",
           main = "Network diagram of AFL players", submain = paste0("Random subset of ",sample_size," pairwise player combinations")) %>% 
  visIgraphLayout(layout = "layout_nicely") %>%
  visGroups(groupname = "1", color = "#A09BE7") %>% 
  visGroups(groupname = "2", color = "#FF686B") %>%
  visGroups(groupname = "3", color = "#861657") %>% 
  visGroups(groupname = "4", color = "#93E1D8") %>%
  visGroups(groupname = "5", color = "#2274A5")
