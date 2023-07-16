### Lesson 4.1: Joins and shape
## New functions: left_join(), right_join(), inner_join(), full_join(), semi_join(), anti_join(), gather(), spread()

## Fisheries relational database

# In this module we will examine fisheries stock assessment data and identify collapsed fisheries. We will be using the RAM Legacy Database, a massive dataset of fisheries metrics across the globe:

# https://www.ramlegacy.org/

# The RAM database has over 50 tables with a range of sizes and shapes, and we will be using several of them. The chief table we will be utilizing has over 1 million rows. Each of the tables are connected by a series of IDs that can be used to merge data appropriately. To manage this massive relational dabase, we will first get comfortable with the join operations in dplyr. 

## Joins

# There is a great cheat-sheet that goes over joins and other data shaping functions: In the RStudio top menu: Help -> Cheatsheets -> Data Transformation with dplyr or here: https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# Here we will learn how to merge data with the join functions of the dplyr package. There are 2 types of joins: 

# -Mutating joins add new variables to one table from matching observations in another table
# -Filtering joins filter observations from one table based on whether or not they match an observation in the other table

# dplyr join functions:

# -Mutating joins:
#     --left_join: Join matching rows from data_2 to data_1
#     --right_join: Join matching rows from data_1 to data_2
#     --inner_join: Join data. Retain only rows in both sets.
#     --full_join: Join data. Retail all values, all rows. 
# -Filtering joins:
#     --semi_join: Return all rows in data_1 that have a match in data_2.
#     --anti_join: Return all rows in data_1 that do not have a match in data_2. 

# We will examine these different types of joins using very simple example data.

# Example data:

library(tidyverse)

data1 = data.frame(ID = 1:2, 
                   X1 = c("a1", "a2"),
                   stringsAsFactors=FALSE)
data2 = data.frame(ID = 2:3,
                   X2 = c("b1", "b2"), 
                   stringsAsFactors=FALSE)

# The figure below illustrates what our two data frames look like and how we can merge them based on the different join functions of the dplyr package. On the top of the figure you can see the structure of our example data frames. Both data frames contain two columns: The ID and one variable. Note that both data frames have the ID No. 2 in common. On the bottom row of the figure you can see how each of the join functions merges our two example data frames. 

in_join = inner_join(data1, data2) 
print(in_join)
# ID X1 X2
# 1  2 a2 b1

lft_join = left_join(data1, data2)
print(lft_join)
# ID X1   X2
# 1  1 a1 <NA>
# 2  2 a2   b1

rt_join = right_join(data1, data2)
print(rt_join)
# ID   X1 X2
# 1  2   a2 b1
# 2  3 <NA> b2

ful_join = full_join(data1, data2)
print(ful_join)
# ID   X1   X2
# 1  1   a1 <NA>
# 2  2   a2   b1
# 3  3 <NA>   b2

sem_join = semi_join(data1, data2)
print(sem_join)
# ID X1
# 1  2 a2

ant_join = anti_join(data1, data2)
print(ant_join)
# ID X1
# 1  1 a1


## Left joins

# This is the join that I use most frequently. The left_join() returns all rows from data1 and all columns from data1 and data2. Rows in data1 with no match in data2 will have NA values in the new columns. If there are multiple matches between data1 and data2, all combinations of the matches are returned. If the joining variable that links your two data frames ... ?, you don't need to explicitly identify what that joining variable is. 

# 3 equivalent ways to perform the left_join():

# Without specifying the joining variable:
data12_left = left_join(data1, data2)
print(data12_left)
# ID X1   X2
# 1  1 a1 <NA>
# 2  2 a2   b1

# Joining, by "ID" - explicitly specifying the joining variable:
data12_left = left_join(data1, data2, by="ID")
print(data12_left)
# ID X1   X2
# 1  1 a1 <NA>
# 2  2 a2   b1

# With piping
data12_left = data1 %>%
  left_join(data2, by="ID")
data12_left
# ID X1   X2
# 1  1 a1 <NA>
# 2  2 a2   b1

# The function right_join() is equivalent to left_join() but reverses the order of the 2 data frames (i.e. all of the rows in the "right" or second data frame are preserved). 


## Inner join

# The function inner_join() returns all rows from data1 where there are matching values in data2, and all columns from data1 and data2. If there are multiple matches between data1 and data2, all combinations of the matches are returned. 

data12_inner = data1 %>%
  inner_join(data2, by="ID")
data12_inner
# ID X1 X2
# 1  2 a2 b1


## Full join

# The function full_join() returns returns all rows and columns from both data1 and data2. The function returns NA for any missing values. 

data12_full = data1 %>%
  full_join(data2, by="ID")
data12_full
# ID   X1   X2
# 1  1   a1 <NA>
# 2  2   a2   b1
# 3  3 <NA>   b2

## Semi join

# The function semi_join() is a filtering function, so no new columns are created in data1, but rows are removed from data1. semi_join() returns all rows from data1 where there are matching values in data2, but only keeps the columns in data1. A semi join will never duplicate rows of data1, even if there is more than one matching row in data2. 

data12_semi = data1 %>%
  semi_join(data2, by="ID")
data12_semi
# ID X1
# 1  2 a2


## Anti join

# The anti_join() function returns all rows from data1 where there are NOT matching values in data2, keeping just the columns from data1.

data12_anti = data1 %>%
  anti_join(data2, by="ID")
data12_anti
# ID X1
# 1  1 a1


## Words of wisdom

# Whenever I use a join function, I always check the dimensions using dim() of my data frames before and after the joins to make sure I understand what rows and columns were added. You can also use the summary() function, or use filter() combined with is.na() to check for NAs so that you have a good sense of what data are missing after a join. 


## Exercise 4.1: Let's imagine you had a month of scuba survey data where each row was a different fish that was observed on a rocky reef. The fish survey data includes the fish's common name, size, date, reef site, and observation ID. Then you have a second data frame that has encyclopedia-type data downloaded from fishbase.org with common name, genus, species, trophic level, and maximum length. If your goal was to add the genus and species information to your survey data and then count the total number of each species observed at a reef, what join would you use? What variable would you join the data frames by? What would happen if there were multiple rows in your fishbase data frame corresponding to the same common name (perhaps one row included a max length estimate pulled from Miller et al. and another row included a max length pulled from Garcia et al.)? Could that mess up your scuba survey analysis?

# Erin would use left_join(survey_data, fish_base_data, by="common_name") so that all the survey data was retained, and add in the fish base data wherever it is available. Then she would use dim() to compare the number of rows in her survey data before and after the join. If her merged data was longer, she would either:

# 1. If she really cared about max length, she would comb through the fish base data frame and remove the duplicate rows based on which max length study was more relevant to her analysis. 
# 2. If she wanted only the genus and species info anyways, she would use distinct(observation_id) to remove duplicate rows of observations - then check again to make sure that the number of rows in her survey data frame was equal to the number of rows before the join function. 


## Switching between long and wide data frames

# The functions pivot_longer() and pivot_wider() (formally called gather() and spread()) in dplyr are used to change the shape of a table from wide to long and from long to wide. This can be an important step for preparing your data for specific types of analysis. For example, a lot of PCA/EOF/nmds analysis packages require data to be in a wide format where different types of observations have their own columns. However, many of ggplot's really slick features work best when data is in a long format. 

# To look at these two functions, we'll create some more example data. Let's pretend you are counting invertebrates that fall inside of a quadrat in the intertidal. Your data might look like this:

survey = data.frame(quadrat_id = c(101, 102, 103, 104),
                    barnacle_n = c(2, 11, 8, 27), 
                    chiton_n = c(1, 0, 0, 2), 
                    mussel_n = c(0, 1, 1, 4))

# This is the "wide" format, because each type of observation has its own column. The pivot_longer() function is used to convert this data to the long format. You will select a subset of columns to stack on top of each other. Set the names_to parameter to the name of the column that contains the old column names. Set the values_to parameter to the name of the column that contains the elements associated with each former column. Note that the columns that are not selected in pivot_longer() will be retained, and their values will be repeated. 

long = survey %>%
  pivot_longer(c("barnacle_n", "chiton_n", "mussel_n"), names_to="taxon", values_to="counts")
head(long)
## # A tibble: 6 x 3
##   quadrat_id taxon      counts
##        <dbl> <chr>       <dbl>
## 1        101 barnacle_n      2
## 2        101 chiton_n        1
## 3        101 mussel_n        0
## 4        102 barnacle_n     11
## 5        102 chiton_n        0
## 6        102 mussel_n        1



head(survey)
## quadrat_id barnacle_n chiton_n mussel_n
## 1        101          2        1        0
## 2        102         11        0        1
## 3        103          8        0        1
## 4        104         27        2        4


## Exercise 1.2: When the invertebrate survey is in the original (wide) format, use ggplot to create a scatter plot where the quadrat_id is along the x-axis and the count data is along the y-axis. Give each different taxon a different point color. Now create the same exact plot using the survey data in the long format. If you wanted to build a linear model to predict the number of barnacles in a quadrat as a function of the number of other invertebrates that were found in the quadrat (i.e. chitons and mussels), what table shape would you need, long or wide?

# We need to write a new geom_point() line for each x/y combo (i.e. each species)
dev.new() 
ggplot(data=survey) + 
  geom_point(aes(x=quadrat_id, y=barnacle_n), color='red') +
  geom_point(aes(x=quadrat_id, y=chiton_n), color='green') + 
  geom_point(aes(x=quadrat_id, y=mussel_n), color='blue')

# Look how easy it is to plot when your data is in the long format:
dev.new()
ggplot(aes(x=quadrat_id, y=counts, color=taxon), data=long) +
  geom_point()



## Acknowledgments

# This lesson and the helpful graphics were adapted from Joachim Schork's blog: https://statisticsglobe.com/r-dplyr-join-inner-left-right-full-semi-anti





















