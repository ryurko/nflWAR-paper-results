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

# Now for RBs
rb_war_seasons <- map_dfr(c(2009:2017),
                          function(x) {
                            readRDS(paste0("data/season_estimates/wpa_based/qb_perc/",
                                           "wpa_model_results_", x, ".rds")) %>%
                              pluck("RB_table") %>%
                              mutate(season = x)
                          })

# For WRs
wr_war_seasons <- map_dfr(c(2009:2017),
                          function(x) {
                            readRDS(paste0("data/season_estimates/wpa_based/qb_perc/",
                                           "wpa_model_results_", x, ".rds")) %>%
                              pluck("WR_table") %>%
                              mutate(season = x)
                          })

# For TEs
te_war_seasons <- map_dfr(c(2009:2017),
                          function(x) {
                            readRDS(paste0("data/season_estimates/wpa_based/qb_perc/",
                                           "wpa_model_results_", x, ".rds")) %>%
                              pluck("TE_table") %>%
                              mutate(season = x)
                          })


# Join the different tables together only selected the necessary rows then
# filtering to only look at the 2017 season:
war_leaders_17 <- select(qb_war_seasons, Player_ID_Name, season, total_WAR, rush_WAR, air_WAR, yac_WAR) %>%
  bind_rows(select(rb_war_seasons, Player_ID_Name, season, total_WAR, rush_WAR, air_WAR, yac_WAR),
            select(wr_war_seasons, Player_ID_Name, season, total_WAR, rush_WAR, air_WAR, yac_WAR),
            select(te_war_seasons, Player_ID_Name, season, total_WAR, rush_WAR, air_WAR, yac_WAR)) %>%
  filter(season == 2017) %>%
  separate(Player_ID_Name, sep = c("-"), into = c("player", "num1", "num2"), remove = FALSE) %>%
  arrange(desc(total_WAR)) %>%
  slice(1:25) %>%
  mutate(player = fct_reorder(player, total_WAR))

# Plot the leaders:
war_leaders_17 %>%
  select(player, total_WAR, rush_WAR, air_WAR, yac_WAR) %>%
  gather(type, war, -player) %>%
  mutate(player = factor(player, levels = levels(war_leaders_17$player)),
         type = factor(type, levels = c("air_WAR", "yac_WAR", 
                                        "rush_WAR", "total_WAR"))) %>%
  ggplot(aes(x = war, y = player, color = type)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 2) +
  scale_color_manual(values = c("darkslateblue", "darkorange4", "seagreen4",
                                "black")) +
    labs(x = "WAR", color = "WAR type", y = "Player",
         title = "NFL WAR leaders in 2017 season",
         subtitle = "(WPA-based WAR estimates)",
         caption = "Data from NFL.com via nflscrapR; WAR from https://github.com/ryurko/nflscrapR-data") +
    theme_bw() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.caption = element_text(size = 12),
          legend.title = element_text(size = 14))

qb_war_season_summary <- qb_war_seasons %>%
  arrange(season) %>%
  filter(season >= 2015) %>%
  #mutate(is_tyrod = if_else(grepl(x = Player_ID_Name, pattern = "T.Taylor"), "Yes", "No")) %>%
  dplyr::select(Player_ID_Name, season, total_WAR, rush_WAR, air_WAR, yac_WAR)

