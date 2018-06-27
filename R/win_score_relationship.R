# File with script for displaying the relationship between score differential
# and the number of wins for teams in individual seasons

# Access tidyverse:
# install.packages("tidyverse")
library(tidyverse)

# Access nflWAR:
# install.packages("devtools")
# devtools::install.packages("ryurko/nflWAR")
library(nflWAR)

# Get the nine season summaries for the 32 teams using the get_season_summary()
# function in the nflWAR package:
season_scorediffs <- get_season_summary(c(2009:2017))

# Plot points and lines for each of these seasons showing the relationship:
season_scorediffs %>%
  ggplot(aes(x = Total_Score_Diff, y = Wins,
             color = as.factor(Season))) + 
  geom_point() + 
  geom_smooth(method="lm", se = FALSE) + 
  labs(x = "Regular season season score differential",
       y = "Regular season wins",
       color = "Season") +
  viridis::scale_color_viridis(discrete = TRUE, 
                               guide = guide_legend(ncol = 9)) + 
  theme_bw() +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = "bottom")

