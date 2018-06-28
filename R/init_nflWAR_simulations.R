# File generates and save nflWAR season simulations

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Access nflWAR:
# install.packages("devtools")
# devtools::install_github("ryurko/nflWAR")
library(nflWAR)

# ------------------------------------------------------------------------------
# Create the list of model formulas to be used, following the nflWAR paper and
# specifying formulas for air and yac passing separately, along with formulas
# for both QB rushing and non-QB rushing separately.

# Next for WPA-based:
wpa_formula_list <- list("air_formula" = 
                           as.formula(airWPA_Result ~ Home_Ind + Shotgun_Ind + 
                                        No_Huddle_Ind + QBHit + Receiver_Position + 
                                        PassLocation + Rush_EPA_Att + 
                                        (1|Passer_ID_Name) + (1|Receiver_ID_Name) + 
                                        (1|DefensiveTeam)),
                         "yac_formula" = 
                           as.formula(yacWPA_Result ~ Home_Ind + Shotgun_Ind + 
                                        No_Huddle_Ind + QBHit + AirYards*Receiver_Position + 
                                        PassLocation + Rush_EPA_Att + (1|Passer_ID_Name) + 
                                        (1|Receiver_ID_Name) + (1|DefensiveTeam)),
                         "qb_rush_formula" = as.formula(WPA ~ Home_Ind + 
                                                          Shotgun_Ind + 
                                                          No_Huddle_Ind + 
                                                          Pass_EPA_Att +
                                                          (1|Rusher_ID_Name) + 
                                                          (1|DefensiveTeam)),
                         "main_rush_formula" = as.formula(WPA ~ Home_Ind + 
                                                            Shotgun_Ind + 
                                                            No_Huddle_Ind + 
                                                            Rusher_Position + 
                                                            Pass_EPA_Att +
                                                            (1|Team_Side_Gap) + 
                                                            (1|Rusher_ID_Name) + 
                                                            (1|DefensiveTeam)))

# ------------------------------------------------------------------------------
# The following code generates the single season simulations for each year from
# 2009 to 2017 for the WPA-based approach using the percentage cutoff for QBs.

walk(c(2009:2017), function(x) {
  # Load the stored season results from the right folder, to ensure that the
  # same players are used for replacement level estimates:
  season_results <- readRDS(paste("data/season_estimates/wpa_based/qb_perc/wpa_model_results_", 
                                  as.character(x), ".rds", sep = ""))
  
  # Create the pipeline expression to get the results in a simulation by resampling
  # at the drive level:
  generate_war_results <- . %>%
    resample_season(drive_level = 1) %>%
    prepare_model_data() %>%
    add_position_tables() %>%
    add_replacement_level_sim(season_results) %>%
    join_position_statistics() %>%
    estimate_player_value_added(wp_model_formula_list, return_models = 0) %>%
    calculate_above_replacement() %>%
    convert_prob_to_wins()
  
  # Simulate the results:
  sim_results <- x %>%
    get_pbp_data() %>%
    add_positions(x) %>%
    add_model_variables() %>%
    simulate_season_statistics(1000, generate_war_results) %>%
    combine_simulations()
  
  # Save 
  saveRDS(sim_results, file = paste("data/simulations/wpa_based/qb_perc/wpa_model_sim_results_", 
                                    as.character(x), ".rds", sep = ""))
  print(paste("Finished simulation for year ", as.character(x), sep = ""))
})

