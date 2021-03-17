## 17 March 2021
## 4.2: Fisheries relational database


## -----------------------------------------------------------------------------------------
load('data/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData')


## ---- message=FALSE-----------------------------------------------------------------------
library(tidyverse)
library(AER) #dispersiontest


## -----------------------------------------------------------------------------------------
# Join timeseries, tsmetrics (time series metadata) and stock tables
fish = timeseries %>%
  left_join(stock, by=c("stockid","stocklong")) %>%
  left_join(tsmetrics, by=c("tsid" = "tsunique")) 
glimpse(fish)

# Status of the stock assessments
unique(fish$state)

# Find the best "total catch" metrics
fish_catch = fish %>% 
  filter(tsid == "TCbest-MT",  # Grab the best TotalCatch estimate (in metric tons)
         state != "Deprecated") # Remove stocks that are deprecated

# Some stocks in timeseries are subject to multiple assessments
length(unique(fish_catch$assessid))
length(unique(fish_catch$stockid))

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

# Histogram of stock assessment lengths
hist(fish_max_assess$assessment_length)

# use semi_join to filter out assessments in fish_catch that are NOT the longest assessment
fish_catch_max_assess = fish_catch %>%
  semi_join(fish_max_assess, by=c("stocklong", "assessid"))

# This plot has LOTS of data - make sure you remove the legend !!
ggplot() +
  geom_line(aes(x=tsyear, y=tsvalue, color=stockid), data=fish_catch_max_assess) +
  theme(legend.position = "none") +
  ggsave('figures/total_catch_all_stocks.png', device="png", height=4, width=7, units="in")

# Fishery with heighest annual catch
fish_catch_max_assess %>% filter(tsvalue == max(tsvalue, na.rm=TRUE))


## -----------------------------------------------------------------------------------------
# What regions have Atlantic cod stock assessments?
cod_regions = fish_catch_max_assess %>% 
  filter(scientificname == "Gadus morhua") %>%
  distinct(region)

# Sum best Total Catch estimates for Cod across all Canada East Coast stock assessments       
cod = fish_catch_max_assess %>% 
  filter(scientificname == "Gadus morhua",
         region == "Canada East Coast") %>%
  group_by(tsyear) %>%
  summarise(total_catch = sum(tsvalue, na.rm=TRUE)) 

# Plot Canada East Coast cod total catch time series
ggplot(aes(x=tsyear, y=total_catch), data=cod) + 
  geom_line() +
  labs(x= "Year", y= "Total Catch (Metric Tons)", 
       title = "Cod Total Catch in East Canadian Coast")


## -----------------------------------------------------------------------------------------
# Find the historical max total catch for each year in the time series
# Define collapse as a total catch <= 10% of the historical max catch
# cummax() in row i provides the max value in rows 0 - i
cod_collapse = cod %>%
  mutate(historical_max_catch = cummax(total_catch),
         collapse = total_catch <= 0.1*historical_max_catch) 

# What year did the collapse happen?
cod_collapse_year = cod_collapse %>% 
  filter(collapse==TRUE) %>% 
  summarize(tsyear=min(tsyear)) %>% 
  .$tsyear

# Plot the catch time series and the collapse year
ggplot() + 
  geom_line(aes(y=total_catch, x=tsyear, color=collapse), data=cod_collapse) +
  geom_vline(xintercept = cod_collapse_year) + # Draws vertical line
  scale_x_continuous(breaks=c(seq(0,2015,10))) + # Add more breaks on x axis
  xlab("Total catch (Mt)") + ylab("Year") + ggtitle("East Canada Cod")


## -----------------------------------------------------------------------------------------
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

ggplot(data = collapse_ts, aes(x=first_collapse_yr, y=ratio_ever_collapsed)) +
  geom_line()

