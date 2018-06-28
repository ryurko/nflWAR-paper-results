# File generates the figures from the single season nflWAR estimates, specifically
# for the 2017 season.

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Access ggridges:
# install.packages("ggridges")
library(ggridges)

# Access ggrepel:
# install.packages("ggrepel")
library(ggrepel)

# Load each of the different 2017 estimates:
wpa_war_season_2017_qb_one <- 
  readRDS("data/season_estimates/wpa_based/qb_one/wpa_model_results_2017.rds")
wpa_war_season_2017_qb_perc <- 
  readRDS("data/season_estimates/wpa_based/qb_perc/wpa_model_results_2017.rds")
epa_war_season_2017_qb_one <- 
  readRDS("data/season_estimates/epa_based/qb_one/epa_model_results_2017.rds")
epa_war_season_2017_qb_perc <- 
  readRDS("data/season_estimates/epa_based/qb_perc/epa_model_results_2017.rds")

# ------------------------------------------------------------------------------
# First calculate the number of replacement level players by position:

# QBs by percentile:
wpa_war_season_2017_qb_perc$QB_table %>%
  filter(Player_Model_ID == "Replacement_QB") %>%
  nrow()
# 25
wpa_war_season_2017_qb_perc$QB_table %>%
  filter(Player_Model_ID == "Replacement_QB") %>%
  nrow(.) / nrow(wpa_war_season_2017_qb_perc$QB_table)
# 0.3521127

# QBs by one game:
wpa_war_season_2017_qb_one$QB_table %>%
  filter(Player_Model_ID == "Replacement_QB") %>%
  nrow()
# 25
wpa_war_season_2017_qb_one$QB_table %>%
  filter(Player_Model_ID == "Replacement_QB") %>%
  nrow(.) / nrow(wpa_war_season_2017_qb_one$QB_table)

# RBs (same number for both rushing and replacement):
wpa_war_season_2017_qb_one$RB_table %>%
  filter(Player_Model_ID_Rush == "Replacement_RB_rush") %>%
  nrow()
# 52
wpa_war_season_2017_qb_one$RB_table %>%
  filter(Player_Model_ID_Rush == "Replacement_RB_rush") %>%
  nrow(.) / nrow(wpa_war_season_2017_qb_one$RB_table)
# 0.3513514

# WRs for receiving:
wpa_war_season_2017_qb_one$WR_table %>%
  filter(Player_Model_ID_Rec == "Replacement_WR_rec") %>%
  nrow()
# 73
wpa_war_season_2017_qb_one$WR_table %>%
  filter(Player_Model_ID_Rec == "Replacement_WR_rec") %>%
  nrow(.) / nrow(wpa_war_season_2017_qb_one$WR_table)
# 0.3631841

# TEs for receiving:
wpa_war_season_2017_qb_one$TE_table %>%
  filter(Player_Model_ID_Rec == "Replacement_TE_rec") %>%
  nrow()
# 45
wpa_war_season_2017_qb_one$TE_table %>%
  filter(Player_Model_ID_Rec == "Replacement_TE_rec") %>%
  nrow(.) / nrow(wpa_war_season_2017_qb_one$TE_table)
# 0.412844

# WRs and TEs for rushing:
wpa_war_season_2017_qb_one$WR_table %>%
  filter(Player_Model_ID_Rush == "Replacement_WR_TE_rush") %>%
  nrow()
# 170
wpa_war_season_2017_qb_one$TE_table %>%
  filter(Player_Model_ID_Rush == "Replacement_WR_TE_rush") %>%
  nrow()
# 108
(170 + 108) / (nrow(wpa_war_season_2017_qb_one$WR_table) + 
                 nrow(wpa_war_season_2017_qb_one$TE_table))
# 0.8967742

# ------------------------------------------------------------------------------
# Next compare the distributions of the different types of WAR for each position.
# The extended edition of the paper, and supplementary materials in the JQAS
# submission includes the comparison of the two types of QB replacement. The 
# JQAS main submission only includes the ten-percent cutoff rule, with the figure
# in the paper showing this version of QB along with the other positions.

# First create a table that has all of the WPA-based WAR values for each of the 
# posiitions using the 10% cutoff for QB replacement:

wpa_war_position_tables <- wpa_war_season_2017_qb_perc$QB_table %>%
  dplyr::select(Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR, total_WAR) %>%
  bind_rows(dplyr::select(wpa_war_season_2017_qb_perc$RB_table,
                          Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR,
                          total_WAR),
            dplyr::select(wpa_war_season_2017_qb_perc$WR_table,
                          Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR,
                          total_WAR),
            dplyr::select(wpa_war_season_2017_qb_perc$TE_table,
                          Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR,
                          total_WAR)) %>%
  gather(war_type, value, -Player_ID_Name, -Position) %>%
  mutate(war_base = "wpa")

# Next EPA-based:
epa_war_position_tables <- epa_war_season_2017_qb_perc$QB_table %>%
  dplyr::select(Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR, total_WAR) %>%
  bind_rows(dplyr::select(epa_war_season_2017_qb_perc$RB_table,
                          Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR,
                          total_WAR),
            dplyr::select(epa_war_season_2017_qb_perc$WR_table,
                          Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR,
                          total_WAR),
            dplyr::select(epa_war_season_2017_qb_perc$TE_table,
                          Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR,
                          total_WAR)) %>%
  gather(war_type, value, -Player_ID_Name, -Position) %>%
  mutate(war_base = "epa")


# Stack them together and view the ridge plots for total_WAR by position,
# comparing the two types of WAR values
wpa_war_position_tables %>%
  bind_rows(epa_war_position_tables) %>%
  filter(war_type == "total_WAR") %>%
  ggplot(aes(x = value, y = Position, fill = war_base)) +
  scale_fill_manual(values = c("darkslateblue", "darkorange4"),
                    labels = c("EPA-based", "WPA-based")) +
  geom_density_ridges(alpha = 0.5, rel_min_height = 0.000005, scale = 1) +
  scale_y_discrete(expand = c(0.01, 0)) +
  theme_bw() +
  labs(x = "WAR", y = "Position", 
       fill = "Type of WAR") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))

# Comparison of the two ways of determining replacement level for QBs
qb_perc_wpa_war_tables <- wpa_war_season_2017_qb_perc$QB_table %>%
  dplyr::select(Player_ID_Name, air_WAR, yac_WAR, rush_WAR, total_WAR) %>%
  mutate(qb_type = "qb_perc",
         war_base = "WPA-based")
qb_perc_epa_war_tables <- epa_war_season_2017_qb_perc$QB_table %>%
  dplyr::select(Player_ID_Name, air_WAR, yac_WAR, rush_WAR, total_WAR) %>%
  mutate(qb_type = "qb_perc",
         war_base = "EPA-based")

qb_one_wpa_war_tables <- wpa_war_season_2017_qb_one$QB_table %>%
  dplyr::select(Player_ID_Name, air_WAR, yac_WAR, rush_WAR, total_WAR) %>%
  mutate(qb_type = "qb_one",
         war_base = "WPA-based")
qb_one_epa_war_tables <- epa_war_season_2017_qb_one$QB_table %>%
  dplyr::select(Player_ID_Name, air_WAR, yac_WAR, rush_WAR, total_WAR) %>%
  mutate(qb_type = "qb_one",
         war_base = "EPA-based")

qb_perc_epa_war_tables %>%
  bind_rows(qb_perc_wpa_war_tables, qb_one_wpa_war_tables, qb_one_epa_war_tables) %>%
  gather(war_type, value, -Player_ID_Name, -qb_type, -war_base) %>% 
  filter(war_type == "total_WAR")  %>%
  ggplot(aes(x = value, y = qb_type, fill = war_base)) +
  scale_fill_manual(values = c("darkslateblue", "darkorange4"),
                    labels = c("EPA-based", "WPA-based")) +
  geom_density_ridges(alpha = 0.5, rel_min_height = 0.000005, scale = 1) +
  scale_y_discrete(labels = c("One QB",
                              "10% cutoff"), expand = c(0.01, 0)) +
  theme_bw() +
  labs(x = "WAR", y = "QB replacement level definition", 
       fill = "Type of WAR") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18))
  
# ------------------------------------------------------------------------------

# Barcharts of the top five players for each position based on total WAR in 
# the 2017 season. First thing is to grab the top five for each position based
# on the total WAR (using the WPA-based method with 10% QB cutoff):

top_five_war_table <- wpa_war_season_2017_qb_perc$QB_table %>%
  dplyr::select(Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR, total_WAR) %>%
  bind_rows(dplyr::select(wpa_war_season_2017_qb_perc$RB_table,
                          Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR,
                          total_WAR),
            dplyr::select(wpa_war_season_2017_qb_perc$WR_table,
                          Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR,
                          total_WAR),
            dplyr::select(wpa_war_season_2017_qb_perc$TE_table,
                          Player_ID_Name, Position, air_WAR, yac_WAR, rush_WAR,
                          total_WAR)) %>%
  # Modify the name:
  mutate(Player_Name = substr(Player_ID_Name, 1, nchar(Player_ID_Name) - 11)) %>%
  # Group by the position:
  group_by(Position) %>%
  # Grab the top five for each position based on total WAR
  arrange(desc(total_WAR)) %>%
  slice(1:5) %>%
  ungroup() %>%
  # Relevel the Player_Name based on total_WAR:
  mutate(Player_Name = fct_reorder(factor(Player_Name), total_WAR))


top_five_war_table %>%
  # Drop the total_WAR column and make the table in long format to plot:
  dplyr::select(-Player_ID_Name, -total_WAR) %>%
  gather(war_type, value, -Player_Name, -Position) %>%
  mutate(war_type = factor(war_type, levels = c("air_WAR", "yac_WAR",
                                                "rush_WAR"))) %>%
  # Now generate the stacked bar charts:
  ggplot(aes(x = factor(Player_Name, 
                        levels = levels(top_five_war_table$Player_Name)),
                        y = value, fill = war_type)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("darkslateblue", "darkorange4", "seagreen4"),
                    labels = c(" air WAR ", " yac WAR ", " rush WAR ")) +
  labs(x = "Player", y = "Wins above replacement") + 
  coord_flip() + 
  facet_wrap(~ Position, ncol = 2, scales = "free") +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_text(size = 18),
        legend.title = element_blank(),
        legend.position = "bottom")
  
# ------------------------------------------------------------------------------
# Create efficiency plots using the iPA values from the model estimates.

# First display the QB air against yac values:
wpa_war_season_2017_qb_perc$QB_table %>%
  dplyr::select(Player_Model_ID, air_iPA, yac_iPA) %>%
  mutate(Player_Name = ifelse(Player_Model_ID == "Replacement_QB",
                              "Replacement level",
                              substr(Player_Model_ID, 1, 
                                     nchar(Player_Model_ID) - 11)),
         repl_ind = factor(ifelse(Player_Name == "Replacement level", 1, 0))) %>%
  distinct() %>%
  ggplot(aes(x = yac_iPA, y = air_iPA)) +
  geom_text_repel(aes(label = Player_Name, color = repl_ind), size = 6) +
  scale_color_manual(values = c("black", "darkred"), guide = FALSE) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  theme_bw() +
  labs(x = "yac iPA", y = "air iPA") +
  annotate("text", x = -.001, y = .0025, size = 10, 
           label = "paste(italic(Gunslingers))", parse = TRUE,
           color = "darkslateblue") +
  annotate("text", x = .0015, y = -0.002, size = 10, 
           label = "paste(italic('Short but accurate'))",
           parse = TRUE, color = "darkorange4") +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18))

# Next display RB efficiency from air + yac against rush with certain players
# marked for convenience:
rb_rush_rec_table <- wpa_war_season_2017_qb_perc$RB_table %>%
  dplyr::select(Player_Model_ID_Rush, Player_ID_Name, Player_Model_ID_Rec, 
                air_iPA, yac_iPA, rush_iPA) %>%
  mutate(Player_Name = ifelse(Player_Model_ID_Rush == "Replacement_RB_rush" & 
                              Player_Model_ID_Rec == "Replacement_RB_rec", 
                              "Replacement level", 
                              substr(Player_ID_Name, 1, 
                                     nchar(Player_ID_Name) - 11)),
         air_yac_iPA = air_iPA + yac_iPA,
         repl_ind = factor(ifelse(Player_Name == "Replacement level", 1, 0))) %>%
  distinct(Player_Name, rush_iPA, air_yac_iPA, repl_ind) 

rb_rush_rec_table %>%
  ggplot(aes(x = rush_iPA, y = air_yac_iPA)) + 
  geom_point(size = 3, alpha = .7) +
  geom_text_repel(data = filter(rb_rush_rec_table, 
                                Player_Name %in% c("A.Kamara",
                                                   "Replacement level",
                                                   "T.Gurley",
                                                   "E.Elliott",
                                                   "Jo.Howard",
                                                   "C.Thompson",
                                                   "C.Hyde",
                                                   "D.Sproles",
                                                   "D.Lewis",
                                                   "M.Ingram")),
                    aes(label = Player_Name, color = repl_ind), size = 6,
                  segment.size = 1, point.padding = .25,
                  segment.color = "gray") +
  scale_color_manual(values = c("black", "darkred"), guide = FALSE) + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  theme_bw() +
  labs(x = "rush iPA", y = "air iPA + yac iPA") +
  annotate("text", x = -.001, y = .0025, size = 10, 
           label = "paste(italic('Pass-catchers'))", parse = TRUE,
           color = "darkslateblue") +
  annotate("text", x = .001, y = -0.002, size = 10, 
           label = "paste(italic('Pure rushers'))",
           parse = TRUE, color = "darkorange4") +
  theme(axis.text = element_text(size = 16), 
        axis.title = element_text(size = 18))
  
