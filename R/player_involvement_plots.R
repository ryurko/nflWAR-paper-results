# File to generate the comparison of the position involvement for a team's plays

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Access nflWAR:
# install.packages("devtools")
# devtools::install.packages("ryurko/nflWAR")
library(nflWAR)

# Access ggridges:
# install.packages("ggridges")
library(ggridges)

# Create the position team tables for each season using the pipeline of functions
# available in the nflWAR package, first for QB:
qb_team_tables <- map_dfr(c(2009:2017), 
                          # For each season
                          function(x) {
                            # Get the play-by-play data
                            get_pbp_data(x) %>% 
                              # Add positions
                              add_positions(x) %>% 
                              # Add necessary model variables
                              add_model_variables() %>% 
                              # Set up the data to extract the QB table
                              prepare_model_data() %>%
                              # Select the necessary columns:
                              create_QB_team_table() %>%
                                     mutate(Season = x) %>%
                                     dplyr::select(Season, Position, Perc_Total_Plays)
                            })

# For RB:
rb_team_tables <- map_dfr(c(2009:2017), 
                          function(x) {
                            get_pbp_data(x) %>% 
                              add_positions(x) %>% 
                              add_model_variables() %>% 
                              prepare_model_data() %>%
                              # Exact same process but create the RB table:
                              create_pos_team_table("RB") %>%
                              mutate(Season = x) %>%
                              dplyr::select(Season, Position, 
                                            Perc_Total_Plays)
                            })

# For WR:
wr_team_tables <- map_dfr(c(2009:2017), 
                          function(x) {
                            get_pbp_data(x) %>% 
                              add_positions(x) %>% 
                              add_model_variables() %>% 
                              prepare_model_data() %>%
                              # Same but WR
                              create_pos_team_table("WR") %>%
                              mutate(Season = x) %>%
                              dplyr::select(Season, Position, Perc_Total_Plays)
                            })

# TE:
te_team_tables <- map_dfr(c(2009:2017), 
                          function(x) {
                            get_pbp_data(x) %>% 
                              add_positions(x) %>% 
                              add_model_variables() %>% 
                              prepare_model_data() %>%
                              create_pos_team_table("TE") %>%
                              mutate(Season = x) %>%
                              dplyr::select(Season, Position, Perc_Total_Plays)
                            })

# Row bind together then make the ridge plots compare the 
# distributions for each year:

qb_team_tables %>%
  bind_rows(rb_team_tables, wr_team_tables, te_team_tables) %>%
  ggplot(aes(y = Position, x = Perc_Total_Plays, fill = Position,
             height = ..density..)) +
  geom_density_ridges(stat = "density", alpha = 0.7, scale = 1.5) +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") + 
  facet_wrap(~ Season, ncol = 3) +
  labs(x = "Proportion of offensive plays a player is directly involved in",
       y = "Position", fill = "Position") + 
  theme(axis.text.x = element_text(size = 14, angle = 90),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text = element_text(size = 18),
        legend.position = "bottom") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0))

