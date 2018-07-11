# File examines the residuals of the WAR models for the 2017 season

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Access magrittr:
# install.packages("magrittr")
library(magrittr)

# Load the 2017 season WPA-based model (with QB percentage cutoff):
wpa_war_season_2017_qb_perc <- 
  readRDS("data/season_estimates/wpa_based/qb_perc/wpa_model_results_2017.rds")

# ------------------------------------------------------------------------------

# Create data frames to store the fitted values and residuals for each of the 
# types of models:

air_model_resid_df <- data.frame(resid = resid(wpa_war_season_2017_qb_perc$air_model, 
                                               type = "pearson"),
                                 fitted_vals = fitted(wpa_war_season_2017_qb_perc$air_model)) %>%
  mutate(model_type = "air_model")
yac_model_resid_df <- data.frame(resid = resid(wpa_war_season_2017_qb_perc$yac_model, 
                                               type = "pearson"),
                                 fitted_vals = fitted(wpa_war_season_2017_qb_perc$yac_model)) %>%
  mutate(model_type = "yac_model")
main_rush_model_resid_df <- data.frame(resid = resid(wpa_war_season_2017_qb_perc$main_rush_model, 
                                                     type = "pearson"),
                                       fitted_vals = fitted(wpa_war_season_2017_qb_perc$main_rush_model)) %>%
  mutate(model_type = "main_rush_model")
qb_rush_model_resid_df <- data.frame(resid = resid(wpa_war_season_2017_qb_perc$qb_rush_model, 
                                                   type = "pearson"),
                                     fitted_vals = fitted(wpa_war_season_2017_qb_perc$qb_rush_model)) %>%
  mutate(model_type = "qb_rush_model")

# Create the facet labels:
model_facet_labels <- as_labeller(c(`air_model` = "Air WAR model",
                                    `yac_model` = "YAC WAR model",
                                    `main_rush_model` = "Non-QB rush WAR model",
                                    `qb_rush_model` = "QB rush WAR model"))

# Stack the model dataframes together and create the facetted chart:
air_model_resid_df %>%
  bind_rows(yac_model_resid_df, main_rush_model_resid_df, qb_rush_model_resid_df) %>%
  ggplot(aes(x = fitted_vals, y = resid)) + 
  geom_point(size = 3, alpha = 0.4, color = "darkblue") +
  labs(x = "Fitted values", y = "Residuals") +
  facet_wrap(~ model_type, labeller = model_facet_labels, ncol = 2,
             scales = "free") +
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        strip.background = element_blank(),
        strip.text = element_text(size = 18))

