# Create fishery collapse table

library(tidyverse)

# https://www.ramlegacy.org/
load('data/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData')

# Find all stocks that have collapsed
collapse = timeseries_values_views %>% 
  filter(!is.na(TCbest)) %>%  # Remove NAs (which can't be ignored with cummax())
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest),
         current_collapse = TCbest < 0.10 * historical_max_catch) %>%
  ungroup()

