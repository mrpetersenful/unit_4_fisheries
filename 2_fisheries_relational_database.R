## 17 March 2021
## 4.2: Fisheries relational database

## In this unit we're going to be using the RAM Legacy Database. The RAM Legacy 
## Stock Assessment Database is a compilation of stock assessment results for 
## commercially exploited marine populations from around the world. It's inspired
## by Dr. Ransom A. Myers' original stock-recruitment database, which is no longer
## being updated. 

## Go to the RAM Legacy website and click through to download from Zenodo. The data
## (rather inefficiently) is delivered in multiple formats simultaneously, including
## Excel files and RData files. I'm going to load the RData file using the load()
## function. 

## Note: Usually when receiving Excel files, I convert them to .csv files and read
## them in with read.csv(), but there is also an R package to load Excel files 
## directly into R called readxl. 

load('data/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData')

## The RAM data is structured as a large relational database which contains many
## different tables of different sizes and shapes, and the tables are related to each
## other through a series of different ids. The database has over 50 tables and some 
## tables have over 1 million rows. This data (and many other super valuable massive
## datasets like this) is difficult to work with and maneuver at first. I'm going to
## show you what metadata files I used to get familiar with the database so that
## I could start this fisheries analysis. 

## From the Database Quick Guide document, we find out that the biometrics table 
## describes all parameter types available in the bioparams table, and the tsmetrics
## table describes all time series types available in the timeseries table. Then if
## we look in the Database Structure document, there is a map that generally shows
## how the major types of tables are connected. 

## Looking deeper in the Database Structure document, there is a table called 
## "Table Linkages" that lists which IDs can be used to link the different tables. 
## For example, to link tsmetrics with timeseries, we must set tsunique to match
## tsid. In other cases, the ID that links tables has the same name. For example, 
## both the timeseries table and stock table have a common ID called stockid. 

### Annual total catch by stock

## We are going to look in the timeseries dataset for annual total catch for each
## fish stock. After exploring the tsmetrics table, I found the tsid TCbest-MT, which 
## is the best available annual catch data (in metric tons). The actual data is all 
## in timeseries, but if we join it with the tsmetrics and stock datasets, then we
## can look at the metadata associated with the timeseries data. 

## We reduce the data by removing stocks in the "Deprecated" state. We also limit 
## each stock to just one assessment to avoid double counting fish. We choose to 
## retain assessment that runs for the longest time period for each stock. 


library(tidyverse)
library(AER) #dispersiontest

## First, I'm going to join the timeseries, tsmetrics (time series metadata) and
## stock tables. 

fish = timeseries %>%
  left_join(stock, by=c("stockid","stocklong")) %>%
  left_join(tsmetrics, by=c("tsid" = "tsunique")) 
glimpse(fish)

## Now I'm going to check the status of the stock assessments. 
unique(fish$state)
## This tells me that there are two states associated with the stock assessments, 
## "Deprecated", and "Current". We only want the current stock assessments for the 
## most accurate numbers. 

## Find the best "total catch" metrics.
fish_catch = fish %>% 
  ## Here I'm going to filter for the TCbest-MT, because that was the best Total
  ## Catch estimate (in metric tons)
  filter(tsid == "TCbest-MT",  
         ## Here, I'm filtering out the "Deprecated" stocks.
         state != "Deprecated")

## Some of the stocks in timeseries have multiple assessments. 
length(unique(fish_catch$assessid))
## assessid has 1751 entries. assessid is a shorthand code name for the fish type.
length(unique(fish_catch$stockid))
## stockid has 942 entries. stockid is a shorter shorthand code name for the fish.

## For a given stock, I want to calculate the time period of each assessment.
## I'm going to choose the assessment that covers the longest time series. If there
## are multiple assessments that cover the same long time period, I only want to 
## keep one. 

fish_max_assess = fish_catch %>% 
  ## Here I'm using group_by to look at only a couple of variables in the table, 
  ## a given stock and assessment. 
  group_by(stocklong, assessid) %>% 
  ## Here I'm telling it to summarize the max and min of the years of the time series.
  summarize(max_tsyear = max(tsyear), min_tsyear = min(tsyear)) %>%
  mutate(assessment_length = max_tsyear - min_tsyear) %>%
  ## Here I'm telling it to ungroup the data.
  ungroup() %>%
  ## Now I'm regrouping by stocklong (which is the full fish name). 
  group_by(stocklong) %>%
  ## And now I'm telling it to keep the longest assessment lengths
  filter(assessment_length == max(assessment_length)) %>% 
  ## Now I'm telling it to keep only the first assessment with the longest assessment
  ## time. If there are more than 2 stocklong with the same ID, keep_all tells it 
  ## to only keep the first set.
  distinct(stocklong, .keep_all=TRUE) %>% 
  ## By using select(), I'm keeping only the following variables in my summary table.
  select(stocklong, assessid, assessment_length)
## Now my fish_max_assess is a data tbl that has the fish id, the assessment id, and
## the amount of time of the assessment. 

## Now I want to visualize the amount of time of the max assessment tbl that I just
## made, so I'm going to check out the assessment length variable in that table. 
hist(fish_max_assess$assessment_length)


## Now I'm going to filter out the assessments in fish_catch that are NOT the longest
## assessment. I'm going to use a semi_join for this, because it will only join the 
## data tables with what rows are in data table 2, but will only keep the values in 
## data table 1. The common variables that we are joining by are "stocklong" and 
## "assessid". 

fish_catch_max_assess = fish_catch %>%
  semi_join(fish_max_assess, by=c("stocklong", "assessid"))

## Now I'm going to plot the fish catch with the max time length. So on the x-axis, 
## I'm going to put year, on the y-axis, I'm going to put the length of the time
## series data, and I'm going to sort the stock ids by color in the graph. 
## This plot has LOTS of data - make sure you remove the legend !!
ggplot() +
  geom_line(aes(x=tsyear, y=tsvalue, color=stockid), data=fish_catch_max_assess) +
  ## This is where I'm removing the legend. 
  theme(legend.position = "none") +
  ggsave('figures/total_catch_all_stocks.png', device="png", height=4, width=7, 
         units="in")

## Now I'm going to figure out where the fishery with the highest annual catch is. 
fish_catch_max_assess %>% filter(tsvalue == max(tsvalue, na.rm=TRUE))
## So the fishery with the highest annual catch is Peru in 1970 with Peruvian
## anchoveta. 


### Cod collapse

## Now that we've created a nice data set of all the best available time series of 
## total catch for fisheries all around the world, we can take a more detailed look 
## at specific stocks. We're going to examine the infamous collapse of the Canadian
## cod stock. 

## Newfoundland and Labrador's historic cod fisheries attracted local and international
## fishing fleets for almost five centuries before the Canadian government shut the 
## industry down indefinitely in July 1992. By then, once-plentiful fish stocks had
## dwindled to near extinction and officials feared they would disappear entirely
## if the fisheries remained open. The moratorium put about 30,000 people in the 
## province out of work and ended a way of life that had endured for generations in
## many port communities. It also made evident the vulnerability of marine resources
## to overexploitation and that existing regulatory regimes were insufficient to 
## protect cod stocks. 

## Let's isolate the cod stock assessments in East Coast Canada and add them together. 
## Then we can plot a time series of the total Canadian East Coast cod stock and 
## try to see what the collapse looked like. 

## Using my max time assessment data, I'm going to figure out which regions have 
## Atlantic cod stock assessments. 
cod_regions = fish_catch_max_assess %>% 
  filter(scientificname == "Gadus morhua") %>%
  distinct(region)

summary(cod_regions) ## This just tells me there are four regions. What are they?
glimpse(cod_regions) ## This tells me the four regions.

## Now I'm going to sum the best Total Catch estimates for Cod across all Canada
## East Coast stock assessments, because we're interested in the Canadian east coast.
cod = fish_catch_max_assess %>% 
  filter(scientificname == "Gadus morhua",
         region == "Canada East Coast") %>%
  group_by(tsyear) %>%
  ## Okay, so now that I've specified which variable I'm in with the group_by() 
  ## function, I'm going to tell it to summarize the total catch using a sum function.
  summarise(total_catch = sum(tsvalue, na.rm=TRUE)) 
## I think that total_catch is just a column that I added into my cod data. 

## Now I'm going to plot the Canada East Coast cod total catch time series.
ggplot(aes(x=tsyear, y=total_catch), data=cod) + 
  geom_line() +
  labs(x= "Year", y= "Total Catch (Metric Tons)", 
       title = "Cod Total Catch in East Canadian Coast")

## A paper by Worm et al. (2006) defines a fishery stock collapse as a decline in 
## total catch to less than 10% of the maximum historical catch. Did the Eastern 
## Canadian cod stock "collapse" according to this definition? We'll use the 
## cummax() function which returns the maximum value in all rows of a data frame 
## previous to a particular row, to find the historical maximum (i.e. the max catch
## observed prior to each year within the analysis). We can identify the year the 
## collapse occurred and add that to our time series plot. 

cod_collapse = cod %>%
  ## Here we're adding a variable historical_max_catch using the cummax() function.
  mutate(historical_max_catch = cummax(total_catch),
         ## and here we're adding a variable collapse that is less than or equal to
         ## 10% of the historical maximum catch. 
         collapse = total_catch <= 0.1*historical_max_catch) 

## Now that we've made our cod collapse tbl, we're going to find out the year that
## the cod collapsed by figuring out which years the stock actually collapsed in 
## our collapse column. 
cod_collapse_year = cod_collapse %>% 
  filter(collapse==TRUE) %>% 
  ## Here, we're adding tsyear variable to tell us what the first year the stock 
  ## collapsed in. 
  summarize(tsyear=min(tsyear)) %>% 
  .$tsyear

## Now I want to plot the catch time series and the collapse year. 
ggplot() + 
  geom_line(aes(y=total_catch, x=tsyear, color=collapse), data=cod_collapse) +
  ## Here, I'm drawing a vertical line on the year the cod collapsed. 
  geom_vline(xintercept = cod_collapse_year) + 
  ## Here I'm adding more breaks on the x-axis. 
  scale_x_continuous(breaks=c(seq(0,2015,10))) +
  xlab("Total catch (Mt)") + ylab("Year") + ggtitle("East Canada Cod")


### Examine fishery collapse across ALL stocks

## Now that we've explored what a stock collapse looks like with cod in Eastern 
## Canadian waters, let's look at the whole RAM dataset and count the number of 
## stocks that have collapsed. How do stock collapse events change through time? 
## How do they change by geographic region? 

## All right. Now I'm going to create a new dataset called collapse that is pulled
## from my fish_catch_max_assess data. 
collapse = fish_catch_max_assess %>% 
  ## First, because cummax() can't ignore NAs, I'm going to pull out all of the NAs.
  filter(!is.na(tsvalue)) %>%  
  ## Now I'm grouping and examining my stock long variable (which is the full name
  ## of the fish). 
  group_by(stocklong) %>%
  ## I'm adding in the historical_max_catch variable again,
  mutate(historical_max_catch = cummax(tsvalue),
         ## then adding a column to see if they are currently collapsed,
         current_collapse = tsvalue < 0.10 * historical_max_catch,
         ## then adding a column to see if they've ever collapsed in the past. I'm 
         ## using the cumsum() variable, which tells me the number of years that the
         ## collapse variable was true. 
         ever_collapsed = cumsum(current_collapse) > 0) %>%
  ## Now I'm going to ungroup my variables so that the data set looks normal again. 
  ungroup()

## Now I'm going to plot a histogram of the first collapse year for all of the different
## stocks. So I'm going to name a new tbl for the collapse year. 
collapse_yr = collapse %>%
  ## Here I say I'm grouping by three things (which are already associated, but this
  ## way I keep the region information). 
  group_by(stockid, stocklong, region) %>% 
  ## Now I'm going to filter out the stocks that have collapsed eve,
  filter(ever_collapsed == TRUE) %>%
  ## and add a variable first_collapse_yr as the first time the stock collapsed,
  summarize(first_collapse_yr = min(tsyear)) %>%
  ## Then I'm going to ungroup my stuff so the tbl goes back to normal. 
  ungroup()

## Let's plot this shit!
ggplot(data = collapse_yr, aes(x=first_collapse_yr)) +
  geom_histogram(color="black", fill="white")

## Now I'm going to create a time series of the number of stocks that have ever
## collapsed out of the total stocks. 

## First I'm creating a new tbl that is the length of as many unique fish ids that 
## we have from our collapse data. 
n_stock_assessments = length(unique(collapse$stockid)) 
## Here I'm adding the collapse time series data by going into our collapse year tbl
collapse_ts = collapse_yr %>%
  ## Now I'm counting the number of unique values in the first_collapse_year column,
  ## which also kind of works like group_by() %>% summarise(n = n()). 
  count(first_collapse_yr) %>%
  ## and adding a column cum_first_collapse_yr, which is the cumulative sum of our
  ## count of the number of times each first_collapse_yr occurred.
  mutate(cum_first_collapse_yr = cumsum(n),
         ## I'm also adding a column ratio_ever_collapsed that is the count of the
         ## first collapse year observations divided by the total number of stock
         ## assessments in our collapse data. 
         ratio_ever_collapsed = cum_first_collapse_yr/n_stock_assessments)

## Now let's plot that shit! :)
ggplot(data = collapse_ts, aes(x=first_collapse_yr, y=ratio_ever_collapsed)) +
  geom_line()


## Exercise 2.1:
## Create the same time series that shows the # of stocks that have collapsed 
## (historically) divided by the total number of stocks that are tracked in the 
## dataset. However, show this plot separately for EACH region. You may need to create
## a new data frame that counts the number of stocks tracked in each region, then
## join that new data frame to the collapse_ts data frame to calculate your ratios. 

## Okay. So I have my n_stock_assessments. But I'm going to fish through (ha) my 
## collapse data to create a new tbl from that. 

n_stock_assessments = collapse %>% 
  ## I'm telling it here to keep the unique stock id values from my collapse data 
  ## set, and if there are duplicates in the stock id, to only keep the first set
  ## of values. 
  distinct(stockid, .keep_all=TRUE) %>%
  ## Now I want to look specifically at the regions.
  group_by(region) %>%
  ## and to create a variable that is the count of stocks collapsed per region.
  summarize(n_stocks_per_region = n())

## Now I'm going to join the stock assessment count data from our collapse column
## into our collapse_ts data. 

collapse_ts = collapse_yr %>%
  left_join(n_stock_assessments) %>%
  ## Now I want to look at the region and number of stocks collapsed per region.
  group_by(region, n_stocks_per_region) %>%
  ## Now I'm counting it and adding that count as a column.
  count(first_collapse_yr) %>%
  ## Now I'm adding in two variables, the cumulative of the first collapsed year,
  ## and the ratio that has every collapsed. 
  mutate(cum_first_collapse_yr = cumsum(n), 
         ratio_ever_collapsed = cum_first_collapse_yr/n_stocks_per_region)

## Now to plot that ish. 

ggplot(data = collapse_ts,
       aes(x=first_collapse_yr, y=ratio_ever_collapsed)) +
  geom_line() +
  ## facet_wrap splits up our data by region and gives us a bunch of different graphs.
  facet_wrap(~region)



