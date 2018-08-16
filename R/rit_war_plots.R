# Code to create additional visuals for RIT presentation highlighting Tyrod Taylor

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# ------------------------------------------------------------------------------

# Read in the QB WAR data table for each season from 2009 to 2017 (wpa-based):
qb_war_seasons <- map_dfr(c(2009:2017),
                          function(x) {
                            readRDS(paste0("data/season_estimates/wpa_based/qb_perc/",
                                           "wpa_model_results_", x, ".rds")) %>%
                              pluck("QB_table") %>%
                              mutate(season = x)
                          })

# Gather the season WAR values of each type starting in 2015 when Tyrod Taylor
# became starting QB for the Buffalo Bills:
qb_war_season_summary <- qb_war_seasons %>%
  arrange(season) %>%
  filter(season >= 2015) %>%
  #mutate(is_tyrod = if_else(grepl(x = Player_ID_Name, pattern = "T.Taylor"), "Yes", "No")) %>%
  dplyr::select(Player_ID_Name, season, total_WAR, rush_WAR, air_WAR, yac_WAR) %>%
  gather(war_type, value, -Player_ID_Name, -season) %>%
  mutate(is_tyrod = if_else(grepl(x = Player_ID_Name, pattern = "T.Taylor"), "Yes", "No"),
         war_type = factor(war_type, levels = c("total_WAR", "air_WAR", 
                                                "yac_WAR", "rush_WAR"),
                           labels = c("Total WAR", "air WAR",
                                      "yac WAR", "rush WAR")))
qb_war_season_summary %>%
  filter(is_tyrod == "No") %>%
  ggplot(aes(x = season, y = value)) + geom_jitter(width = 0.2, height = 0) +
  geom_point(data = filter(qb_war_season_summary, is_tyrod == "Yes"),
             color = "darkblue", size = 6) +
  geom_point(data = filter(qb_war_season_summary, is_tyrod == "Yes"),
             color = "red", size = 4.5) +
  facet_wrap(~ war_type, ncol = 2) +
  theme_bw() +
  scale_x_continuous(breaks = 2015:2017) +
  scale_y_continuous(breaks = -5:5) + 
  labs(
    x = "Season", 
    y = "WAR (WPA-Based)",
    title = "Each Type of QB WAR by Season, 2015-2017",
    subtitle = "Highlighted = Tyrod Taylor",
    caption = "Data from NFL.com via nflscrapR; WAR from https://github.com/ryurko/nflscrapR-data"
  ) + 
  theme(plot.caption = element_text(size = 12),
        strip.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        strip.text = element_text(size = 14))
  
 