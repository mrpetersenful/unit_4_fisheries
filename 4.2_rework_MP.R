### Lesson 4.2: Using joins for a relational database

## Fisheries Data

# In this unit we will be using the RAM Legacy Database:

# https://www.ramlegacy.org/

# The RAM Legacy Stock Assessment Database is a compilation of stock assessment results for commercially exploited marine populations from around the world. It is inspired by Dr. Ransom A. Myers' original stock-recruitment database, which is no longer being updated. 

# Go to the RAM Legacy website and click through to download the latest version of the RAM data from Zenodo. The data (rather inefficiently, if you ask me) is delivered in multiple formats simultaneously, including Microsoft Excel files and RData files. Since we are using R, I'm going to load the RData file using the load() function. 

# Note: Usually when I receive Excel files, I convert them to .csv files and read them in with read.csv(), but there is also an R package to load Excel files directly into R called readxl. 

load('data/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData')

# The RAM data is structured as a large relational database which contains many different tables of different sizes and shapes, and the tables are related to each other through a series of different ids. The database has over 50 tables and some tables have over 1 million rows. This data (and many other super valuable massive datasets just like this) is difficult to work with and maneuver at first. I'm going to show you what metadata files I used to get familiar with the database so that I could start this fisheries analysis. 

# From the Database Quick Guide document, we find out that the biometrics table describes all parameter types available in the bioparams table, and the tsmetrics table describes all time series types available in the timeseries table. A simple version of the most commonly used fisheries metrics by stock and by year is available in timeseries_values_views. Then if we look in the Database Structure document, there is a map that generally shows how the major types of tables are connected.

# Looking deeper in the Database Structure document, there is a table called "Table Linkages" that lists which IDs can be used to link the different tables. For example, both the timeseries_values_views table and stock table have a common ID called stockid. 

# The other good metadata file provided is called Database Table Fields. This is a spreadsheet that provides explanations for the variable names that represent the broad range of fishery metrics presented in this dataset. Now that we have glanced through the metadata, we can start a fisheries analysis. 



















