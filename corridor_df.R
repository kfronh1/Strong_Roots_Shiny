library(tidyverse)
library(janitor)
library(lubridate)

corridor <- read_csv("Corridor.csv") %>%
  clean_names()

projects_all <- read_csv("project_df.csv") %>%
  clean_names() %>%
  mutate(date = lubridate::mdy(date))

