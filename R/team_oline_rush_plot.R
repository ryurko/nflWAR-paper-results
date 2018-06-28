# File generates the O-line figure to compare the values between the 2016 and
# 2017 seasons (just based on the single season estimates).

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Access ggrepel:
# install.packages("ggrepel")
library(ggrepel)

# Access lme4:
# install.packages("lme4")
library(lme4)

# ------------------------------------------------------------------------------

# Load the 2016 and 2017 WPA-based war results (QB replacement doesn't matter):
wpa_war_season_2017 <- readRDS("data/season_estimates/wpa_based/qb_perc/wpa_model_results_2017.rds")
wpa_war_season_2016 <- readRDS("data/season_estimates/wpa_based/qb_perc/wpa_model_results_2016.rds")

# Extract the tPA values for the run-gaps from the 2016 and 2017 models:
calc_team_tpa_sum <- . %>%
  ranef() %>%
  as.data.frame() %>%
  filter(grpvar == "Team_Side_Gap") %>%
  dplyr::select(grp, condval) %>%
  rename(team_side_gap = grp, tpa = condval) %>%
  filter(!(grepl("NA", team_side_gap))) %>%
  mutate(team = str_extract(team_side_gap,"[A-Z]{2,3}-") %>%
           str_extract("[A-Z]{2,3}")) %>%
  # Calculate the team's sum of tPA values:
  group_by(team) %>%
  summarise(tpa_total = sum(tpa))

oline_2016_summary <- wpa_war_season_2016$main_rush_model %>%
  calc_team_tpa_sum() %>%
  rename(tpa_total_2016 = tpa_total) %>%
  # Modify SD to be LAC for easy comparison:
  mutate(team = ifelse(team == "SD", "LAC", team))

oline_2017_summary <- wpa_war_season_2017$main_rush_model %>%
  calc_team_tpa_sum() %>%
  rename(tpa_total_2017 = tpa_total)

oline_2016_summary %>% inner_join(oline_2017_summary, by = "team") %>%
  ggplot(aes(x = tpa_total_2016, y = tpa_total_2017)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = team), size = 8, point.padding = .5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Sum of team's tPA for each run gap in 2016 season",
       y = "Sum of team's tPA for each run gap in 2017 season") +
  theme_bw() + 
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18))
