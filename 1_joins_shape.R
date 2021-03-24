## 15 March 2021
## 4.1: Joins in R


## In this module, we're looking at fisheries stock assessment data and 
## identifying collapsed fisheries. We're going to use the RAM Legacy 
## Database, a massive dataset of fisheries metrics across the globe. 

## The RAM database has over 50 tables with a range of sizs and shapes, 
## we'll be using several of them. The chief table we're utilizing has 
## more than 1 million rows. Each of the tables are connected by a series 
## of IDs that can be used to merge data appropriately. To manage this 
## massive relational database, we'll first get comfortable with join 
## functions in dplyr. 

## There's a good cheatsheet that goes over joins and other data shaping 
## functions: in the top RStudio menu, go to Help, Cheatsheets, Data 
## Transformation with dplyr. 

## Here we're going to learn how to merge data with the join functions of
## the dplyr package. There are 2 types of joins:
#### - Mutating joins add new variables to one table from matching 
####   observations in another table
#### - Filtering joins filter observations from one table based on whether
####   or not they match an observation in the other table. 

## dplyr join functions: 
#### - Mutating joins:
####   -- left_join: Join matching rows from data_2 to data_1
####   -- right_join: Join matching rows from data_1 to data_2
####   -- inner_join: Join data. Retain only rows in both sets.
####   -- full_join: Join data. Retain all values, all rows. 
#### - Filtering joins: 
####   -- semi_join: Return all rows in data_1 that have a match in data_2.
####   -- anti_join: Return all rows in data_1 that do not have a match in
####        data_2.

## We're going to look at these different types of joins using very simple
## example data. 

library(tidyverse)
## Create first example data frame
data1 <- data.frame(ID = 1:2,                      
                    X1 = c("a1", "a2"),
                    stringsAsFactors = FALSE)
## Create second example data frame
data2 <- data.frame(ID = 2:3,                      
                    X2 = c("b1", "b2"),
                    stringsAsFactors = FALSE)

## Both data frames contain two columns: the ID and one variable. Note that
## both data frames have the ID No. 2 in common. On the bottom row of the
## figure you can see how each of the join functions merges our two 
## example data frames. 

### Left joins/right joins:
## This is the join used most frequently by Erin. The left_join() returns
## all rows from data1 and all columns from data1 and data2. Rows in data1
## with no match in data2 will have NA values in the new columns. If there 
## are multiple matches between data1 and data2, all combinations of the 
## matches are returned. If the joining variable links your two data frames, 
## you don't need to explicitly identify what that joining variable is. 

## 3 equivalent ways to perform the left_join():

# Without specifying the joining variable:
data12_left = left_join(data1, data2)
# Explicitly specifying the joining variable:
data12_left = left_join(data1, data2, by="ID")
# With piping
data12_left = data1 %>%
  left_join(data2, by="ID")
data12_left
## In our example new data table, data1 has only rows 1 and 2, where data2
## has rows 2 and 3. We only keep the rows from data1, although all of the
## columns are added from data2. 

## The function right_join() is equivalnt to left_join() but reverses the 
## order of the 2 data frames (i.e., all of the rows in the "right" or 
## second data frame are preserved). 

### Inner join:
## The function inner_join() returns all rows from data1 where there are 
## matching values in data2, and all columns from data1 and data2. If there
## are multiple matches between data1 and data2, all combinations of the
## matches are returned. 

data12_inner = data1 %>%
  inner_join(data2, by="ID")
data12_inner
## This returned row 2 (because it's present in both data1 and data2), and
## all columns from both data1 and data2.


### Full join: 
## The function full_join() returns all rows and columns from both data1 
## and data2. The function returns NA for any missing values. 

data12_inner = data1 %>%
  full_join(data2, by="ID")
data12_inner


### Semi join: 
## The function semi_join() is a filtering function, so no new columns are
## created in data1, but rows are removed from data1. semi_join() returns
## all rows from data1 where there are matching values in data2, but only
## keeps the columns in data1. A semi join will never duplicate rows of
## data1, even if there is more than one matching row in data2. 

data12_semi = data1 %>%
  semi_join(data2, by="ID")
data12_semi

### Anti join:
## The anti_join() function returns all rows from data1 where there are 
## NOT matching values in data2, keeping just the columns from data1. 

data12_anti = data1 %>%
  anti_join(data2, by="ID")
data12_anti

#### WORDS OF WISDOM:
## Whenever a join function is used, you should ALWAYS check the dimensions
## using dim() of the data frames before and after the joins to make sure
## you understand what rows and columns were added. You can also use the
## summary() function, or use filter() combined with is.na() to check for 
## NAs so that you have a good sense of what data are missing after a join.

## Exercise 1.1
## Imagine you had a month of scuba survey data where each row was a different
## fish that was observed on a rocky reef. The fish suvey data includes 
## the fish's common name, size, date, reef site and observation ID. Then
## you have a second data frame that has encyclopedia-type data downloaded
## from fishbase.org with common name, genus, species, trophic level, and
## maximum length. 

## If your goal was to add the genus and species information to your survey
## data, what join would you use? 
### I would use left_join to add in all of the encyclopedia data but retain 
### only the rows from my survey data. 
# left_join(survey_data, fish_base_data, by="common_name")
### I would make sure to check dim() of my survey data to make sure that
### I didn't add or remove rows to my survey data after the join. 

## What would happen if there were multiple rows in your fishbase data frame 
## corresponding to the same common name (perhaps one row included a max
## length estimate pulled from Miller et al. and another row included a
## max length estimate pulled from Garcia et al.)? Could that mess up your
## scuba survey analysis?
### If there were more rows, I would either:
### 1) comb through the fish base data frame and remove the duplicate rows
###    based on which max length study was more relevant to my analysis.
### 2) If I only wanted the genus and species info anyways, I would use
###    distinct(observation_id) to remove duplicate rows of observations
###    -- then I would check again to make sure that the number of rows in
###    my survey data frame is equal to the number of rows before the join
###    function. 


### Switching between long and wide data frames

## The functions pivot_longer() and pivot_wider() (formally called gather()
## and spread()) in dplyr are used to change the shape of a table from wide 
## to long and from long to wide. This can be an important step for preparing 
## your data for specific types of analysis. For example, a lot of PCA/
## EOF/nmds analysis packages require data to be in a wide format where 
## diferent types of observations have their own columns. However, many of 
## ggplot's really slick features work best when data is in a long format.

## To look at these two functions, we'll create some more example data. 
## Let's pretend we're counting invertebrates that fall inside of a quadrat
## in the intertidal. Your data might look like this:

survey = data.frame(quadrat_id = c(101, 102, 103, 104),
                    barnacle_n = c(2, 11, 8, 27),
                    chiton_n = c(1, 0, 0, 2),
                    mussel_n = c(0, 1, 1, 4))
summary(survey)
## This is the "wide" format, because each type of observation has its own
## column. The pivot_longer() function is used to convert this data to the 
## long format. You'll select a subset of columns to stack on top of each
## other. Set the names_to parameter to the name of the column that contains 
## the old column names. Set the values_to parameter to the name of the 
## column that contains the elements associated with each former column.
## Note that the columns that are not selected in pivot_longer() will be 
## retained, and their values will be repeated. 



long = survey %>% 
  pivot_longer(c("barnacle_n", "chiton_n", "mussel_n"), names_to="taxon", 
               values_to="counts")
head(long)
## So this move expanded the 101 quadrat id column to make a new row for 
## each taxon (old column) -- the column from the old table that had the 
## differences were the counts, so that's what we set the values_to thing
## equal to. 

## Then we can take long data and convert it to wide data for analysis with
## the pivot_wider() function. This time, identify the key column that will 
## become column names and the value column that holds the data that will fill
## each new column. 

wide = long %>%
  pivot_wider(names_from=taxon, values_from=counts)
head(wide)
## In this data frame, the rows that we wanted to turn into columns was named
## the taxon column, and the counts column were filled into the cells.


## Exercise 1.2: 
## When the invertebrate survey is in the original (wide) format, use ggplot 
## to create a scatter plot where the quadrat_id is along the x-axis and the 
## count data is along the y-axis. Give each different taxon a different 
## point color. Now create the same exact plot using the survey data in the 
## long format. 

## If you wanted to build a linear model to predict the number of barnacles in
## a quadrat as a function of the number of other invertebrates that were found
## in the quadrat (i.e. chitons and mussels), what table shape would you need, 
## long or wide? 

## Okay, so we need to build a geom_point() line for each x/y combo (i.e., each
## species). 
ggplot(data=survey) +
  geom_point(aes(x=quadrat_id, y=barnacle_n), color='red') +
  geom_point(aes(x=quadrat_id, y=chiton_n), color='green') +
  geom_point(aes(x=quadrat_id, y=mussel_n), color='blue')

## Now, look how easy it is to plot when the data is in the long format:
ggplot(aes(x=quadrat_id, y=counts, color=taxon), data=long) +
  geom_point()



