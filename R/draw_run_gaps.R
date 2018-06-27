# Script to generate the run gap figure in the nflWAR paper, this code is
# originally by Sam Ventura.

# Access tidyverse
# install.packages("tidyverse")
library(tidyverse)

# Initialize data frame with the locations of each position:
position_data <- data.frame(xx = c(-2:3, 0),
                            yy = c(0, 0, 0.01, 0, 0, 0, -1/8),
                            position = c("Left\nTackle", "Left\nGuard", 
                                         "Center\n", "Right\nGuard", 
                                         "Right\nTackle", "Tight\nEnd", "QB"))

# Initialize data frame with the locations of the run gaps:
gap_data <- data.frame(xx = c(-3, -2:3) + 0.5,
                       yy = -1 / 16,
                       gap = c("C Gap\n=\nTackle", "B Gap\n=\nGuard", 
                               "A Gap\n=\nMiddle", "A Gap\n=\nMiddle", 
                               "B Gap\n=\nGuard", "C Gap\n=\nTackle", 
                               "D Gap\n=\nEnd"))

# Visualize the run gaps with the positions:
ggplot() + 
  geom_label(data = position_data, size = 7, 
             fontface = "bold", color = "white", fill = "grey11",
             aes(x = xx, y = yy, label = position)) + 
  geom_label(data = gap_data, size = 4, fontface = "bold", 
             color = "black", fill = "lightblue",
             aes(x = xx, y = yy, label = gap)) + 
  geom_hline(yintercept = 0.05, linetype = "dashed") + 
  annotate(geom = "text", x = 0, y = 0.06, label = "Line of Scrimmage") + 
  theme_void() + 
  ylim(-0.15, 0.1)


