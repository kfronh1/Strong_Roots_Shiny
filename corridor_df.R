library(tidyverse)
library(janitor)


corridor <- read_csv("Corridor.csv") %>%
  clean_names()
