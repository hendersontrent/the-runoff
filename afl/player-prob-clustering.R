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

# Pull data for last season

season_2020 <- pull_afl_data(start_date = "2020-01-01", end_date = "2020-12-01")

#----------------------- Pre processing ----------------------------

#----------------
# Merge and clean
#----------------

tmp1 <- bind_rows(season_2016, season_2017, season_2018, season_2019, season_2020)

the_finals <- c("EF", "SF", "QF", "PF", "GF") # Removes finals as these might bias analysis

# Get averages for each player

player_data <- season_2020 %>%
  filter(round %ni% the_finals) %>%
  mutate(player_name = paste0(first_name, " ", surname)) %>%
  group_by(player_name, playing_for) %>%
  summarise(kicks = mean(kicks),
            marks = mean(marks),
            contested_marks = mean(contested_marks),
            contested_possessions = mean(contested_possessions),
            handballs = mean(handballs)) %>%
  ungroup()

# Identify just top 200 players with highest sum averages

top_player_data <- player_data %>%
  mutate(combined = kicks + marks + handballs + contested_marks + contested_possessions) %>%
  top_n(100, combined) %>%
  dplyr::select(c(player_name, playing_for, combined))

player_data <- player_data %>%
  left_join(top_player_data, by = c("player_name" = "player_name",
                                    "playing_for" = "playing_for")) %>%
  drop_na()

player_data <- mutate(player_data, id = rownames(player_data)) # Helps with LPA merge

#----------------------- Run the LPA -------------------------------

m1 <- player_data %>%
  dplyr::select(-c(id, player_name, playing_for, combined)) %>%
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

# Filter to just Model 1/Classes 6 as this model had the best analytic hierarchy

lpa_outputs_filt <- lpa_outputs %>%
  filter(model_number == "6" & classes_number == 2)

# Join back in to main dataset and retain just the class for each player
# that has the highest probability

final_profiles <- player_data %>%
  mutate(id = as.integer(id)) %>%
  inner_join(lpa_outputs_filt, by = c("id" = "id", "kicks" = "kicks", "marks" = "marks",
                                      "handballs" = "handballs", "contested_marks" = "contested_marks",
                                      "contested_possessions" = "contested_possessions")) %>%
  group_by(id) %>%
  slice(which.max(Probability)) %>%
  ungroup() %>%
  mutate(Class = as.factor(Class)) %>%
  mutate(player_name = make.unique(player_name, sep = "_")) %>% # Some players have the same name
  dplyr::select(-c(id, model_number, classes_number, Class_prob, Probability))

#----------------------- Correlations between players --------------

# Get list of all pairwise player combinations

pairwise_list <- expand.grid(from = final_profiles$player_name, to = final_profiles$player_name) %>%
  filter(from != to)

# Compute correlations

empty_list <- list()

for(r in 1:nrow(pairwise_list)){
  tmp_players <- pairwise_list[r,]
    
  player_from <- final_profiles %>%
    filter(player_name == tmp_players$from)
  
  player_to <- final_profiles %>%
    filter(player_name == tmp_players$to)
  
  player_from_vector <- c(player_from$kicks, player_from$marks, player_from$handballs,
                          player_from$contested_marks, player_from$contested_possessions)
  player_to_vector <- c( player_to$kicks,  player_to$marks,  player_to$handballs,
                         player_to$contested_marks,  player_to$contested_possessions)
  
  if(length(player_from_vector) != length(player_to_vector)){
    stop("Error: Vectors are not equal lengths.")
  } else{
      
    the_corr <- data.frame(from = c(unique(player_from$player_name)),
                           to = c(unique(player_to$player_name)),
                           value = c(cor(player_from_vector, player_to_vector)))
    
    empty_list[[r]] <- the_corr
  }
}

cutoff <- 0.7

pairwise_correlations <- rbindlist(empty_list, use.names = TRUE) %>%
  mutate(value_indicator = case_when(
         value >= -cutoff & value <= cutoff ~ "Drop",
         value < -cutoff                    ~ "Keep",
         value > cutoff                     ~ "Keep")) %>%
  filter(value_indicator == "Keep") %>% # Removes small and moderate correlations to keep network edges manageable
  dplyr::select(-c(value_indicator))

#----------------------- Data visualisation ------------------------

# Requires nodes and edges

nodes <- final_profiles %>%
  mutate(title = paste0("<p><b>",player_name,"</b><br>","Group = ",Class,"</p>")) %>% # HTML label for interactive hover
  dplyr::select(c(player_name, Class, title, combined)) %>%
  mutate(combined = round(rescale(combined, to = c(0,1)), digits = 3)) %>% # Rescale to make nodes a good size on network diagram
  rename(id = player_name,
         group = Class,
         value = combined) %>% # Renames into variables required for JavaScript library
  mutate(group = as.factor(group),
         font.color = "white", # Node labels on plot
         font.size = 30) # Font size needs to be bigger on plot

edges <- pairwise_correlations %>%
  dplyr::select(-c(value))

#-------------------
# Make network graph
#-------------------

player_network_diag <- visNetwork(nodes, edges, height = "1000px", width = "100%",
           main = "Network diagram of core metrics between the top 100 AFL players on average on these metrics over the 2020 season",
           submain = "<br>Groups determined by probabilistic Latent Profile Analysis of average kicks, marks, handballs, contested marks and<br>contested possessions. Node size = Sum of all metric averages.<br>Edge size = Average correlation between players. Correlations < +- 0.7 were filtered out for visual clarity.",
           footer = "Source: CRAN package fitzRoy which pulls data from www.afltables.com and www.footywire.com",
           background = "#E9EAE0") %>% 
  visIgraphLayout(layout = "layout_nicely",
                  smooth = FALSE,
                  randomSeed = 123) %>%
  visGroups(groupname = "1", color = "#A09BE7") %>% 
  visGroups(groupname = "2", color = "#93E1D8") %>%
  visLegend() %>%
  visOptions(selectedBy = "group", 
             highlightNearest = TRUE, 
             nodesIdSelection = TRUE) %>%
  visPhysics(stabilization = FALSE)

# Save to HTML

visSave(player_network_diag, file = "afl/output/player_network_diag.html")
