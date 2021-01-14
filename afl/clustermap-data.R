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

# Pull data all the way back to 2011

years <- c(seq(from = 2011, to = 2020, by = 1))
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
