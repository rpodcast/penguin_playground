library(tidyverse)
library(palmerpenguins)

glimpse(penguins)

penguins %>% distinct(species)

penguins %>% 
  select(where(is.numeric))

penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point(aes(color = island),
             size = 2,
             alpha = 0.5) +
  theme_minimal()

ggplot(penguins, 
       aes(x = .data[[input$x_axis]], y = .data[[input$y_axis]],
           size = input$point_size)) +
  geom_point(aes(color = .data[[input$scatterplot_color]])) +
  theme_bw()

