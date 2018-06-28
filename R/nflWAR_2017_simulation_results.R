# File generates the figures related to the nflWAR simulation results for the
# 2017 season.

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Access ggridges:
# install.packages("ggridges")
library(ggridges)

# Access the teamcolors package by Ben Baumer and Greg Matthews:
# devtools::install_github("beanumber/teamcolors")
library(teamcolors)

# ------------------------------------------------------------------------------

# Load the 2017 simulation results for the WPA-based WAR values using the 
# ten-percent cutoff for QB replacement level:
wpa_model_sim_results_2017 <- readRDS("data/simulations/wpa_based/qb_perc/wpa_model_sim_results_2017.rds")

# Next generate the ridge plots showing the simulation distributions for a 
# selection of QBs in descending order based on total WAR, with separate 
# distributions for each type of WAR:

qb_sims_table_subset <- wpa_model_sim_results_2017$QB_table %>%
  dplyr::select(Player_Model_ID, air_WAR, rush_WAR, yac_WAR, total_WAR) %>%
  # Modify the name:
  mutate(Player_Name = substr(Player_Model_ID, 1, nchar(Player_Model_ID) - 11)) %>%
  # Filter to select QBs for the paper visual (mostly "elite" QBs... and Flacco)
  filter(Player_Name %in% c("R.Wilson", "C.Wentz", "T.Brady", "A.Smith", 
                            "T.Taylor", "D.Prescott", "C.Keenum", "D.Brees", 
                            "J.Winston", "E.Manning", "P.Rivers", "J.Flacco")) %>%
  # Group by each player and find their average total_WAR:
  group_by(Player_Name) %>%
  mutate(mean_total_war = mean(total_WAR, na.rm = TRUE)) %>%
  ungroup() %>%
  # Relevel the Player_Name based on total_WAR:
  mutate(Player_Name = fct_reorder(factor(Player_Name), mean_total_war))

qb_sims_table_subset %>%
  dplyr::select(-total_WAR, -Player_Model_ID, -mean_total_war) %>%
  gather(war_type, value, -Player_Name) %>%
  mutate(war_type = factor(war_type, levels = c("air_WAR", "yac_WAR", "rush_WAR"))) %>%
  ggplot(aes(x = value, 
             y = factor(Player_Name, 
                        levels = levels(qb_sims_table_subset$Player_Name)),
             fill = war_type)) +
  geom_density_ridges(alpha = 0.5, color = "white", 
                      rel_min_height = 0.001, from = -3, to = 6) +
  scale_fill_manual(values = c("darkslateblue", "darkorange4", "seagreen4"),
                    labels = c(" air WAR ", " yac WAR ", " rush WAR ")) +
  labs(x = "Simulated WAR Values",
       y = "QB") +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = "bottom")

# Now the distributions for the top ten RBs:
top_ten_rbs <- wpa_model_sim_results_2017$RB_table %>%
  dplyr::select(Player_Model_ID_Rush, air_WAR, rush_WAR, yac_WAR, total_WAR) %>%
  # Modify the name:
  mutate(Player_Name = substr(Player_Model_ID_Rush, 1,
                              nchar(Player_Model_ID_Rush) - 11)) %>%
  # Group by each player and find their average total_WAR:
  group_by(Player_Name) %>%
  summarise(mean_total_war = mean(total_WAR, na.rm = TRUE)) %>%
  # Grab the top ten based on average total_WAR across the simulations:
  arrange(desc(mean_total_war)) %>%
  slice(1:10) %>%
  mutate(Player_Name = fct_reorder(factor(Player_Name), mean_total_war)) %>%
  pull(Player_Name)
  
wpa_model_sim_results_2017$RB_table %>%
  dplyr::select(Player_Model_ID_Rush, air_WAR, rush_WAR, yac_WAR) %>%
  # Modify the name:
  mutate(Player_Name = substr(Player_Model_ID_Rush, 1,
                              nchar(Player_Model_ID_Rush) - 11)) %>%
  # Filter to select top ten RBs based on average of the simulations:
  filter(Player_Name %in% top_ten_rbs) %>%
  dplyr::select(-Player_Model_ID_Rush) %>%
  gather(war_type, value, -Player_Name) %>%
  mutate(war_type = factor(war_type, levels = c("air_WAR", "yac_WAR", "rush_WAR"))) %>%
  ggplot(aes(x = value, 
             y = factor(Player_Name, levels = levels(top_ten_rbs)),
             fill = war_type)) +
  geom_density_ridges(alpha = 0.5, color = "white", 
                      rel_min_height = 0.001, from = -1.5, to = 3) +
  scale_fill_manual(values = c("darkslateblue", "darkorange4", "seagreen4"),
                    labels = c(" air WAR ", " yac WAR ", " rush WAR ")) +
  labs(x = "Simulated WAR Values",
       y = "QB") +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) + 
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = "bottom")

# ------------------------------------------------------------------------------
# Comparison of Kamara and Hunt:
kc_color <- teamcolors %>%
  filter(league == "nfl", 
         name == "Kansas City Chiefs") %>%
  pull(primary)

no_color <- teamcolors %>%
  filter(league == "nfl", 
         name == "New Orleans Saints") %>%
  pull(primary)

hunt_kamara_sim_table <- wpa_model_sim_results_2017$RB_table %>%
  select(Player_Model_ID_Rush, total_WAR) %>%
  # Modify the name:
  mutate(Player_Name = substr(Player_Model_ID_Rush, 1,
                              nchar(Player_Model_ID_Rush) - 11)) %>%
  # Grab Kamara and Hunt
  filter(Player_Name %in% c("A.Kamara", "K.Hunt")) %>%
  select(-Player_Model_ID_Rush) %>%
  # Now make them both columns by first marking the simulation number
  # based on the row, and then spread the long data wide:
  group_by(Player_Name) %>%
  mutate(sim = row_number()) %>%
  spread(Player_Name, total_WAR) %>%
  ungroup() %>%
  mutate(higher_war = ifelse(`A.Kamara` > `K.Hunt`, "Kamara", "Hunt"))

# Simulation results?
table(hunt_kamara_sim_table$higher_war) / nrow(hunt_kamara_sim_table)
# Hunt Kamara 
# 0.292  0.708

# Visualize their comparison in the same way as openWAR for baseball:
hunt_kamara_sim_table %>%
  ggplot(aes(x = `A.Kamara`, y = `K.Hunt`, color = higher_war)) + 
  geom_point(size = 4, alpha = 0.6) + 
  theme_bw() + 
  labs(x = "Simulated 2017 WAR: Alvin Kamara",
       y = "Simulated 2017 WAR: Kareem Hunt",
       color = "Better WAR?") +
  scale_color_manual(values = c(kc_color, no_color)) +
  annotate("text", label = "Kamara better 70.8%", x = 3.3, y = 0.1, size = 10) +
  annotate("text", label = "Hunt better 29.2%", x = .75, y = 3.1, size = 10) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18),
        legend.position = "bottom")


