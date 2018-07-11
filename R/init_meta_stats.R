# File computes the autocorrelation between the 2016 and 2017 seasons for 
# QB WAR along with passer rating and adjusted net yards per attempt, as well as
# for RB WAR compared to success rate and yards per attempt.

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Load the 2017 season WPA-based model (with QB percentage cutoff):
wpa_war_season_2016_qb_perc <- 
  readRDS("data/season_estimates/wpa_based/qb_perc/wpa_model_results_2016.rds")

# Load the 2017 season WPA-based model (with QB percentage cutoff):
wpa_war_season_2017_qb_perc <- 
  readRDS("data/season_estimates/wpa_based/qb_perc/wpa_model_results_2017.rds")

# ------------------------------------------------------------------------------

# Note that these are only first steps of meta analysis for these football stats,
# a thorough review will require its own paper and body of work.

# ------------------------------------------------------------------------------
# First for QBs (excluding replacement level), grab the necessary columns, rename
# to designate the year, then join together players that are in both years:

# QBs (Remove replacement level):

qb_2016_table <- wpa_war_season_2016_qb_perc$QB_table %>%
  filter(Player_Model_ID != "Replacement_QB") %>%
  select(Player_ID_Name, air_iPA, yac_iPA, rush_iPA,
         air_WAR, yac_WAR, rush_WAR, total_WAR,
         Passer_Rating, PACR, AdjNetYardsAtt,
         Pass_Success_Rate, Pass_WPA) %>%
  rename(air_iPA_16 = air_iPA,
         yac_iPA_16 = yac_iPA,
         rush_iPA_16 = rush_iPA,
         air_WAR_16 = air_WAR,
         yac_WAR_16 = yac_WAR,
         rush_WAR_16 = rush_WAR,
         total_WAR_16 = total_WAR,
         Passer_Rating_16 = Passer_Rating,
         PACR_16 = PACR,
         AdjNetYardsAtt_16 = AdjNetYardsAtt,
         Pass_Success_Rate_16 = Pass_Success_Rate,
         Pass_WPA_16 = Pass_WPA)

qb_2017_table <- wpa_war_season_2017_qb_perc$QB_table %>%
  filter(Player_Model_ID != "Replacement_QB") %>%
  select(Player_ID_Name, air_iPA, yac_iPA, rush_iPA,
         air_WAR, yac_WAR, rush_WAR, total_WAR,
         Passer_Rating, PACR, AdjNetYardsAtt,
         Pass_Success_Rate, Pass_WPA) %>%
  rename(air_iPA_17 = air_iPA,
         yac_iPA_17 = yac_iPA,
         rush_iPA_17 = rush_iPA,
         air_WAR_17 = air_WAR,
         yac_WAR_17 = yac_WAR,
         rush_WAR_17 = rush_WAR,
         total_WAR_17 = total_WAR,
         Passer_Rating_17 = Passer_Rating,
         PACR_17 = PACR,
         AdjNetYardsAtt_17 = AdjNetYardsAtt,
         Pass_Success_Rate_17 = Pass_Success_Rate,
         Pass_WPA_17 = Pass_WPA)

qb_join_table <- qb_2016_table %>%
  inner_join(qb_2017_table, by = "Player_ID_Name") %>%
  mutate(Player_Name = substr(Player_ID_Name, 1, nchar(Player_ID_Name) - 11)) %>%
  select(-Player_ID_Name)

cor(qb_join_table$total_WAR_16, qb_join_table$total_WAR_17)
# 0.5982524
cor(qb_join_table$Passer_Rating_16, qb_join_table$Passer_Rating_17)
# 0.4779661
cor(qb_join_table$AdjNetYardsAtt_16, qb_join_table$AdjNetYardsAtt_17)
# 0.2948111

# Next for RBs:
rb_2016_table <- wpa_war_season_2016_qb_perc$RB_table %>%
  filter(Player_Model_ID_Rush != "Replacement_RB_rush" | Player_Model_ID_Rec != "Replacement_RB_rec") %>%
  select(Player_ID_Name, air_iPA, yac_iPA, rush_iPA,
         air_WAR, yac_WAR, rush_WAR, total_WAR,
         Rush_Yards_per_Rush,
         Rush_Success_Rate, Rush_WPA) %>%
  rename(air_iPA_16 = air_iPA,
         yac_iPA_16 = yac_iPA,
         rush_iPA_16 = rush_iPA,
         air_WAR_16 = air_WAR,
         yac_WAR_16 = yac_WAR,
         rush_WAR_16 = rush_WAR,
         total_WAR_16 = total_WAR,
         Rush_Yards_per_Rush_16 = Rush_Yards_per_Rush,
         Rush_Success_Rate_16 = Rush_Success_Rate,
         Rush_WPA_16 = Rush_WPA)

rb_2017_table <- wpa_war_season_2017_qb_perc$RB_table %>%
  filter(Player_Model_ID_Rush != "Replacement_RB_rush" | Player_Model_ID_Rec != "Replacement_RB_rec") %>%
  select(Player_ID_Name, air_iPA, yac_iPA, rush_iPA,
         air_WAR, yac_WAR, rush_WAR, total_WAR,
         Rush_Yards_per_Rush,
         Rush_Success_Rate, Rush_WPA) %>%
  rename(air_iPA_17 = air_iPA,
         yac_iPA_17 = yac_iPA,
         rush_iPA_17 = rush_iPA,
         air_WAR_17 = air_WAR,
         yac_WAR_17 = yac_WAR,
         rush_WAR_17 = rush_WAR,
         total_WAR_17 = total_WAR,
         Rush_Yards_per_Rush_17 = Rush_Yards_per_Rush,
         Rush_Success_Rate_17 = Rush_Success_Rate,
         Rush_WPA_17 = Rush_WPA)

rb_join_table <- rb_2016_table %>%
  inner_join(rb_2017_table, by = "Player_ID_Name") %>%
  mutate(Player_Name = substr(Player_ID_Name, 1, nchar(Player_ID_Name) - 11)) %>%
  select(-Player_ID_Name)

cor(rb_join_table$total_WAR_16, rb_join_table$total_WAR_17)
# 0.3681273 
cor(rb_join_table$Rush_Success_Rate_16, rb_join_table$Rush_Success_Rate_17)
# 0.205955
cor(rb_join_table$Rush_Yards_per_Rush_16, rb_join_table$Rush_Yards_per_Rush_17)
# 0.2148362

