# Create fishery collapse table
# 2021-1-29

library(tidyverse)

# https://www.ramlegacy.org/
load('data/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData')

# Join timeseries, tsmetrics (time series metadata) and stock tables
fish = timeseries %>%
  left_join(stock, by=c("stockid","stocklong")) %>%
  left_join(tsmetrics, by=c("tsid" = "tsunique")) 

# Find the best "total catch" metrics
fish_catch = fish %>% 
  filter(tsid == "TCbest-MT",  # Grab the best TotalCatch estimate (in metric tons)
         state != "Deprecated") # Remove stocks that are deprecated

# for a given stock, calculate the time period of each assessment 
# choose the assessment that covers the longest time series 
# if multiple assessments cover the same long time period, only keep one
fish_max_assess = fish_catch %>% 
  group_by(stocklong, assessid) %>% # For a given stock and assessment
  summarize(max_tsyear = max(tsyear), min_tsyear = min(tsyear)) %>%
  mutate(assessment_length = max_tsyear - min_tsyear) %>%
  ungroup() %>%
  group_by(stocklong) %>%
  filter(assessment_length == max(assessment_length)) %>% # keep longest assessment length
  distinct(stocklong, .keep_all=TRUE) %>% # only keep first assessment with max assessment length
  select(stocklong, assessid, assessment_length)

# use semi_join to filter out assessments in fish_catch that are NOT the longest assessment
fish_catch_max_assess = fish_catch %>%
  semi_join(fish_max_assess, by=c("stocklong", "assessid"))

# Find all stocks that have collapsed
collapse = fish_catch_max_assess %>% 
  filter(!is.na(tsvalue)) %>%  # Remove NAs (which can't be ignored with cummax())
  group_by(stocklong) %>%
  mutate(historical_max_catch = cummax(tsvalue),
         current_collapse = tsvalue < 0.10 * historical_max_catch,
         ever_collapsed = cumsum(current_collapse) > 0) %>%
  ungroup()

# Find the year each stock collapsed for the first time
collapse_yr = collapse %>%
  group_by(stockid, stocklong, region) %>% # really just groups by stockid, but retains region
  filter(ever_collapsed == TRUE) %>%
  summarize(first_collapse_yr = min(tsyear)) %>%
  ungroup()

# Plot a histogram of first collapse year
ggplot(data = collapse_yr, aes(x=first_collapse_yr)) +
  geom_histogram(color="black", fill="white")

# Create a time series of # of stocks ever collapsed / total stocks
n_stock_assessments = length(unique(collapse$stockid)) # Total number of unique stocks in our data
collapse_ts = collapse_yr %>%
  count(first_collapse_yr) %>%
  mutate(cum_first_collapse_yr = cumsum(n),
         ratio_ever_collapsed = cum_first_collapse_yr/n_stock_assessments)


