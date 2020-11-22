#------------------------------------------
# This script sets out to pull the data
# needed for a a clustermap of metrics that
# feed the 
#
# NOTE: This script requires setup.R to
# have been run first
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 22 November 2020
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

# Pull data for the last 10 seasons

season_2011 <- pull_afl_data(start_date = "2011-01-01", end_date = "2011-12-01")
season_2012 <- pull_afl_data(start_date = "2012-01-01", end_date = "2012-12-01")
season_2013 <- pull_afl_data(start_date = "2013-01-01", end_date = "2013-12-01")
season_2014 <- pull_afl_data(start_date = "2014-01-01", end_date = "2014-12-01")
season_2015 <- pull_afl_data(start_date = "2015-01-01", end_date = "2015-12-01")
season_2016 <- pull_afl_data(start_date = "2016-01-01", end_date = "2016-12-01")
season_2017 <- pull_afl_data(start_date = "2017-01-01", end_date = "2017-12-01")
season_2018 <- pull_afl_data(start_date = "2018-01-01", end_date = "2018-12-01")
season_2019 <- pull_afl_data(start_date = "2019-01-01", end_date = "2019-12-01")
season_2020 <- pull_afl_data(start_date = "2020-01-01", end_date = "2020-12-01")

#---------------------- Pre processing -----------------------------

#----------------
# Merge and clean
#----------------

# Merge datasets

tmp1 <- bind_rows(season_2011, season_2012, season_2013, season_2014, season_2015,
                  season_2016, season_2017, season_2018, season_2019, season_2020)

# Aggregate data to match level

the_finals <- c("EF", "SF", "QF", "PF", "GF") # Removes finals as these might bias analysis

clustermap_data <- tmp1 %>%
  filter(round %ni% the_finals) %>%
  group_by(season, round, home_team) %>%
  summarise(kicks = sum(kicks),
            marks = sum(marks),
            goals = sum(goals),
            behinds = sum(behinds),
            handballs = sum(handballs),
            hit_outs = sum(hit_outs),
            tackles = sum(tackles),
            rebounds = sum(rebounds),
            inside_50s = sum(inside_50s),
            clearances = sum(clearances),
            clangers = sum(clangers),
            frees_for = sum(frees_for),
            frees_against = sum(frees_against),
            contested_possessions = sum(contested_possessions),
            uncontested_possessions = sum(uncontested_possessions),
            contested_marks = sum(contested_marks),
            marks_inside_50 = sum(marks_inside_50),
            bounces = sum(bounces),
            goal_assists = sum(goal_assists)) %>%
  ungroup() %>%
  dplyr::select(-c(season, round, home_team))

# Compute correlation matrix

corr <- round(cor(clustermap_data), 2)

my_plot <- ggcorrplot::ggcorrplot(corr, hc.order = TRUE, colors = c("#2274A5", "white", "#FF686B")) +
  labs(title = "Correlogram of match-level sums")

#---------------------- Export plot --------------------------------

CairoPNG("afl/output/correlogram.png", 600, 600)
print(my_plot)
dev.off()

# Turn into interactive object and save as HTML to embed in website

heatmap_plot <- ggplotly(my_plot)
htmlwidgets::saveWidget(as_widget(heatmap_plot), "afl/output/heatmap-plot.html")
