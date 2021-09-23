library(tidyverse)
library(palmerpenguins)

glimpse(penguins)

penguins %>% distinct(species)
