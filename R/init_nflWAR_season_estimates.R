# File generates the single season nflWAR estimates

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Access nflWAR:
# install.packages("devtools")
# devtools::install_github("ryurko/nflWAR")
library(nflWAR)

# ------------------------------------------------------------------------------

# Using the nflWAR package, create the list of functions for finding 
# replacement level players. Will create two lists, to compare two approaches
# for finding replacement level QBs: (1) based on the percentage of plays cutoff,
# and (2) considering NFL level to only be the QBs that attempted the first pass
# of the first game in the season for a team, and all other QBs as replacement
# level. Will also use the roster based approach for the other positions by
# using the create_league_replacement_fn() in the nflWAR package, that takes in
# a roster cutoff, position, and variable to use returning a function that finds
# the replacment level players given the inputs.

# First the percentage based approach for QBs:
repl_fn_list_qb_perc <- list("find_replacement_QB" = 
                               create_percentage_replacement_fn("Perc_Total_Plays", .1),
                             "find_replacement_RB_rec" = 
                               create_league_replacement_fn(3, "RB", "Targets"), 
                             "find_replacement_WR_rec" = 
                               create_league_replacement_fn(4, "WR", "Targets"),
                             "find_replacement_TE_rec" = 
                               create_league_replacement_fn(2, "TE", "Targets"),
                             "find_replacement_RB_rush" = 
                               create_league_replacement_fn(3, "RB",
                                                            "Rush_Attempts"),
                             "find_replacement_WR_TE_rush" = 
                               create_league_replacement_fn(1, "WR",
                                                            "Rush_Attempts",
                                                            combine_wrte = 1))

# Next the game one approach:
repl_fn_list_qb_one <- list("find_replacement_QB" = 
                               find_game_one_replacement_QB,
                             "find_replacement_RB_rec" = 
                               create_league_replacement_fn(3, "RB", "Targets"), 
                             "find_replacement_WR_rec" = 
                               create_league_replacement_fn(4, "WR", "Targets"),
                             "find_replacement_TE_rec" = 
                               create_league_replacement_fn(2, "TE", "Targets"),
                             "find_replacement_RB_rush" = 
                               create_league_replacement_fn(3, "RB",
                                                            "Rush_Attempts"),
                             "find_replacement_WR_TE_rush" = 
                               create_league_replacement_fn(1, "WR",
                                                            "Rush_Attempts",
                                                            combine_wrte = 1))

# ------------------------------------------------------------------------------
# Create the list of model formulas to be used, following the nflWAR paper and
# specifying formulas for air and yac passing separately, along with formulas
# for both QB rushing and non-QB rushing separately.

# First for the EPA-based approach:
epa_formula_list <- list("air_formula" = 
                           as.formula(airEPA_Result ~ Home_Ind + Shotgun_Ind + 
                                        No_Huddle_Ind + QBHit + Receiver_Position + 
                                        PassLocation + Rush_EPA_Att + 
                                        (1|Passer_ID_Name) + (1|Receiver_ID_Name) + 
                                        (1|DefensiveTeam)),
                          "yac_formula" = 
                           as.formula(yacEPA_Result ~ Home_Ind + Shotgun_Ind + 
                                        No_Huddle_Ind + QBHit + AirYards*Receiver_Position + 
                                        PassLocation + Rush_EPA_Att + (1|Passer_ID_Name) + 
                                        (1|Receiver_ID_Name) + (1|DefensiveTeam)),
                           "qb_rush_formula" = as.formula(EPA ~ Home_Ind + 
                                                            Shotgun_Ind + 
                                                            No_Huddle_Ind + 
                                                            Pass_EPA_Att +
                                                            (1|Rusher_ID_Name) + 
                                                            (1|DefensiveTeam)),
                           "main_rush_formula" = as.formula(EPA ~ Home_Ind + 
                                                              Shotgun_Ind + 
                                                              No_Huddle_Ind + 
                                                              Rusher_Position + 
                                                              Pass_EPA_Att +
                                                              (1|Team_Side_Gap) + 
                                                              (1|Rusher_ID_Name) + 
                                                              (1|DefensiveTeam)))

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
# The following code generates the single season estimates for each year from
# 2009 to 2017 for both EPA and WPA based approaches, along with both ways of
# defining replacement level QBs. Each combination relies on similar pipelines
# of nflWAR functions with the pipeline beginning as follows:

init_season_war_data <- function(year) {
  year %>%
  get_pbp_data() %>%
  add_positions(year) %>%
  add_model_variables() %>%
  prepare_model_data() %>%
  add_position_tables() %>%
  join_position_statistics()
}

# Now use pipeline along with each of the options above and generate the 
# results for each season, saving the data of each season and replacement,
# model type, in the proper data folder:

# First for WPA-based with the percentile based QB replacement level:
walk(c(2009:2017), function(x) {
  season_results <- x %>% 
    init_season_war_data() %>%
    find_positional_replacement_level(repl_fn_list_qb_perc) %>%
    estimate_player_value_added(wpa_formula_list) %>%
    calculate_above_replacement() %>%
    convert_prob_to_wins()
  
  saveRDS(season_results, 
          file = 
            paste("data/season_estimates/wpa_based/qb_perc/wpa_model_results_",
                  as.character(x), ".rds", sep = ""))
})

# EPA-based with the percentile based QB replacement level:
walk(c(2009:2017), function(x) {
  season_results <- x %>% 
    init_season_war_data() %>%
    find_positional_replacement_level(repl_fn_list_qb_perc) %>%
    estimate_player_value_added(epa_formula_list) %>%
    calculate_above_replacement() %>%
    convert_points_to_wins(calculate_points_per_win(x))
  
  saveRDS(season_results, 
          file = 
            paste("data/season_estimates/epa_based/qb_perc/epa_model_results_",
                  as.character(x), ".rds", sep = ""))
})

# WPA-based with game one QB replacement level cutoff:
walk(c(2009:2017), function(x) {
  season_results <- x %>% 
    init_season_war_data() %>%
    find_positional_replacement_level(repl_fn_list_qb_one) %>%
    estimate_player_value_added(wpa_formula_list) %>%
    calculate_above_replacement() %>%
    convert_prob_to_wins()
  
  saveRDS(season_results, 
          file = 
            paste("data/season_estimates/wpa_based/qb_one/wpa_model_results_",
                  as.character(x), ".rds", sep = ""))
})

# EPA-based with game one QB replacement level cutoff:
walk(c(2009:2017), function(x) {
  season_results <- x %>% 
    init_season_war_data() %>%
    find_positional_replacement_level(repl_fn_list_qb_one) %>%
    estimate_player_value_added(epa_formula_list) %>%
    calculate_above_replacement() %>%
    convert_points_to_wins(calculate_points_per_win(x))
  
  saveRDS(season_results, 
          file = 
            paste("data/season_estimates/epa_based/qb_one/epa_model_results_",
                  as.character(x), ".rds", sep = ""))
})

