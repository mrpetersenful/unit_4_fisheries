## 22 March 2021
## 4.3: Generalized linear models


### Exploring fisheries data with generalized linear models. 

## We're going to continue to use the RAM Legacy Database for this lesson. 

load('data/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData')

## First I'm going to import the code for the tables that we built in the last class, 
## such as fish_catch_max_assess and collapse. That way we can use the data 
## manipulation work we've already done as a basis for running our models today. 
## I just put all of the code used to generate these tables in their own R script, 
## and now I use the source() function to run that R script and put all of the 
## associated variables in my environment, as an alternative to copy-pasting the code
## we wrote in the last class. 

source('build_collapse_table.R')


### Logistic regression

## Logistic regression is used to model a binary dependent variable or a dependent
## variable that is bound between 0 and 1. 

## Logistic regression can be fit with the glm() function using the binomial link. 
## After the model is fit, model predictions can be made with the predict() function
## just like we did with linear regressions. 

## Logistic regression does not require model residuals to be normally distributed 
## or variance to be constant. AIC can be used for model comparison, i.e., to 
## determine whether the inclusion of an additional independent variable constitutes
## a significantly improved model. McFadden's Pseudo-R^2 can be used to assess 
## fitness, where McFadden claims that a Pseudo-R^2 between 0.2 and 0.4 represents
## an "excellent fit". 

## Which stocks have experienced a collapse? 

## What features of a stock make it more (or less) likely to experience a collapse?
## I find this question fascinating, and I'd love to model the chance of collapse as 
## a function of habitat type, geographic region, primary country that fishes that
## stock, fishing methods and age at sexual maturity. Unfortunately, the RAM data
## doesn't have a lot of that background info for most stocks (see bioparams table). 
## We do consistently have region, i.e. the geographical location of the stock, 
## so let's see if region is a significant explanatory variable for predicting the
## stock's collapse. 

## Since collapse is a yes/no, TRUE/FALSE, 0/1, binomial type of variable, we should
## use logistic regression. 


## Here, I'm going to remove the time series information and collapse the data to 
## "has this stock EVER collapsed?"
model_data = collapse %>%
  ## I'm using the collapse tbl from the last class, and checking the stockid and 
  ## region variables.
  group_by(stockid, region) %>%
  ## I'm going to add a variable, ever_collapsed, to get a yes or no answer in this 
  ## column.
  summarize(ever_collapsed = any(ever_collapsed)) %>%
  ## and now I'm going to ungroup to make sure that the tbl collapse goes back to 
  ## normal. 
  ungroup()
glimpse(model_data)

## Now I'm going to run a logistic regression. 
model_l = glm(ever_collapsed ~ region, data = model_data, family = "binomial")
summary(model_l)
## Hmm, I'm not super sure that I know what I'm looking at with the results. Let's
## keep going. 

## Now I want to make predictions on the probability of a stock collapse by region.
## Here, I'm creating a new tbl from our model_data (yes/no for ever collapsed 
## variable). 
regions = model_data %>% distinct(region)
## Now I'm going to see if I can predict this using the regions data and our logistic
## model. The newdata parameter is the data that I want to use to generate predictions.
## I'm also using the type="response" because I want my responses to be between 0 
## and 1.
model_l_predict = predict(model_l, newdata=regions, type="response", se.fit=TRUE)

## What does our model_l_predict look like?
summary(model_l_predict)
## It looks like there are 19 regions.
glimpse(model_l_predict)
## That gives me no information. So let's look at it a different way. 

## I want to organize my predictions into a tidy table. So I'm going to incorporate 
## my predictionss together with my regions tbl in a new data frame.
collapse_region_predictions = data.frame(region = regions,
                                         predictions = model_l_predict$fit,
                                         se = model_l_predict$se.fit)

## Now I want to visualize it by plotting the predictions and standard error bars.
ggplot(aes(x=region, y=predictions, fill=region), data=collapse_region_predictions) +
  ## Here is where I add in the bars. I'm using stat="identity" because I want the
  ## bars to be as long as our y variable.
  geom_bar(stat="identity") +
  ## Here's where I add in error bars.
  geom_errorbar(aes(ymin=predictions-se, ymax=predictions+se), width=0.2) +
  ## Here's where I tell the x and y to flip so the graph is easier to understand.
  coord_flip() +
  ## Here's where I delete the redundant legend. 
  theme(legend.position = "none")
## Cool. Looks like humans kinda suck. 

## Now I want to calculate McFadden Pseudo-R^2, where 0.2-0.4 = "Excellent fit".
library(pscl)
pscl::pR2(model_l)["McFadden"]

## So our output is 0.1197512, which means it's not an excellent fit. Going back to
## our summary for our model, we have statistically significant negative coefficients
## for the Mediterranean-Black Sea stocks and the West Africa stocks - meaning that
## those stocks are not likely to collapse. The Canada East and West Coast stock 
## groups are almost significant at alpha = 0.05, and since those coefficients are 
## positive, that indicates that stocks are more likely to collapse. There are no
## examples of collapsed stocks in the Pacific Ocean and Indian Ocean regions. 

## Logistic regression can be a good tool for any type of binomial analysis, 
## including binomial categorical variables (like sex), probabilities (including 
## demographic or state space transition probabilities), or presence/absence 
## occurrence data. That's some good shit. 


### Poisson model

## The poisson regression is usually the best choice for modeling count data: discrete
## data with non-negative integer values. The poisson model can also be applied to 
## rate data, as in the count of an event per unit of time, space, etc. The divisor
## of your rate data (i.e. the amount of time, space, etc. sampled) will be treated
## as an "offset" variable in the model. In ecology, poisson regression is appropriate
## for counting individuals in a survey. For example, fish counts along a transect 
## may need to be offset by transect length if the length isn't standard in the 
## survey. However, count of barnacles wouldn't require an offset term if the same
## size quadrat is always used. Number of events can also be modeled with poisson
## regression, such as the number of rainy days per month, with an offset for the 
## total number of days in the month. 

## There are two snags that you can run into with a poisson model: zero inflation and
## overdispersion. 

## Zero inflation can be problematic when there is an excessive amount of zeros in
## your count data. Often, the mechanisms that govern zero-counts may be different 
## from the mechanisms that govern counts of >=1. For example, did you count zero 
## of a certain fish species because the true number in that habitat is pretty low, 
## or because you are in the completely wrong habitat? If you were an omniscient 
## modeler, you would separate out the habitats that are "impossible" to find that
## species, and then run the model on the remaining data where it IS possible to find
## that species. However, it's more likely that you don't know enough to make that 
## distinction (that's why you're conducting these surveys!). So if you have zero-
## inflated data, you'll have to consider removing the zeros and just explicitly
## modeling the positive count data, or designing a model that treats the zeros 
## carefully (see below for more information). 

## Overdispersion occurs when the observed variance is much higher than the mean. 
## After running a poisson model, you should test for overdispersion (I use 
## AER::dispersiontest()). If overdispersion is present, then you should switch to 
## a different distributional family like a quasipoisson (where the variance is 
## assumed to be a linear function of the mean) or a negative binomial (where the
## variance is assumed to be a quadratic function of the mean). 

## Model the period of time that a stock is collapsed

## In the fisheries unit, I was hoping to demonstrate a poisson model where I predicted
## fish catch (i.e. the count of fish caught) with an offset for effort. CPUE (Catch
## Per Unit Effort) is a ubiquitous metric in fishery research. Unfortunately, there
## is very little effort data in the RAM dataset, so I decided to go a different route.

## We can use a poisson distribution to model the count of years that a stock spends
## in the collapsed state. Is the length of time that a stock spends in the 
## "collapsed" state dependent on how frequently it is overfished or how often its
## biomass is below Bmsy (biomass of maximum sustainable yield)?

## U is fishing pressure (often fishing mortalities) and B is biomass. What is 
## U / U_MSY and B / B_MSY?

## First I'm going to calculate the ratio of years that a stock is overfished and
## stock biomass is less than B_MSY.

u_summary = timeseries_values_views %>% 
  left_join(stock, by=c("stockid","stocklong")) %>%
  ## Taking out NA data for those columns.
  filter(!is.na(UdivUmsypref),
         !is.na(BdivBmsypref)) %>%
  ## Looking at variable of stockid and adding columns.
  group_by(stockid) %>%
  summarize(yrs_data = n(),
            ratio_yrs_overfished = sum(UdivUmsypref > 1)/yrs_data,
            ratio_yrs_low_stock = sum(BdivBmsypref < 1)/yrs_data) %>%
  ## and taking out the yrs_data column.
  select(-yrs_data)

## Now I'm going to count the number of years that each stock is collapsed, and I'm 
## going to join with counts of overfished-years and low-stock years in the tbl we 
## just made. 

collapse_summary = collapse %>%
  group_by(stockid, stocklong, region) %>%
  summarize(yrs_data = n(),
            yrs_collapsed = sum(current_collapse)) %>%
  left_join(u_summary, by="stockid")

## Do we have zero-inflation?
table(collapse_summary$yrs_collapsed)
## This says that we have 569 rows where there were 0 years collapsed. 

## Create a zero-truncated data set to demonstrate poisson model. I'm getting rid 
## of the zeros here. 
collapse_summary_zero_trunc = collapse_summary %>% filter(yrs_collapsed>0)

## Now to build our poisson model. 
model_p = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_low_stock, 
              offset(log(yrs_data)), data=collapse_summary_zero_trunc, 
              family="poisson") 
summary(model_p)
## All right, looks like our poisson model is significant. But let's check our other
## parameter for poisson models: overdispersion (where the variance is larger than
## the mean). 

## Okay, so the count of years that a stock was in a collapsed state had tons of zeros,
## and the zero-inflation would make a problem for a poisson model fit. We dealt
## with this by removing the zeros, but we must keep in mind that this fundamentally
## changes the analysis to: "Out of the stocks that have experienced a collapse at
## some point, what drives the length of time they spend collapsed?". This is still
## an interesting (and related) question, but the results should be presented in the
## right context. 

## Do we have overdispersion?
AER::dispersiontest(model_p)$p.value < 0.05 
## TRUE = overdispersed; FALSE = NOT overdispersed ----- Yes, we have overdispersion.

## I'm going to address the overdispersion by using a quasipoisson or negative 
## binomial model. 

model_qp = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_low_stock , 
               offset(log(yrs_data)), data=collapse_summary_zero_trunc, 
               family="quasipoisson") 
summary(model_qp)
## So this shows that the number of years that a stock is overfished does NOT 
## significantly drive the time spent in the collapsed state, but the length of time
## that a stock's biomass falls below BMSY IS a significant driver of the time spent
## in the collapsed state. 

## Now I want to make predictions on the time period that a stock spends collapsed
## as a function of low stock year rates with overfishing rates set to the observed
## median. 

newdata = data.frame(ratio_yrs_low_stock = seq(from=0,to=1,by=0.1),
                     ratio_yrs_overfished = 
                       median(collapse_summary_zero_trunc$ratio_yrs_overfished, 
                              na.rm=TRUE))
model_qp_predict = predict(model_qp, type="response", newdata = newdata, 
                           se.fit=TRUE)

## Now I'm going to organize the predictions into a tidy table. 
collapse_time_predictions = cbind(newdata,
                                  data.frame(predictions = model_qp_predict$fit,
                                             se = model_qp_predict$se.fit))

## And I'm going to plot predictions and SE ribbon.
ggplot() +
  geom_line(aes(x=ratio_yrs_low_stock, y=predictions), 
            data=collapse_time_predictions) +
  geom_ribbon( aes(x=ratio_yrs_low_stock, ymin = predictions-se, 
                   ymax = predictions+se), fill="darkgrey", alpha = .5, 
               data=collapse_time_predictions) +
  geom_point(aes(x=ratio_yrs_low_stock, y=yrs_collapsed), 
             data=collapse_summary_zero_trunc) +
  theme_bw()

## We plotted the quasipoisson model fit across the range of possible low 
## stock time periods, while holding the ratio of time spent in the overfished 
## state constant at the observed median. The plot shows that the predicted time 
## spent in the collapsed state goes from something like 10 to 20 years as the 
## ratio of time spent in the low stock state moves from 0 to 1.

## What fishery has been collapsed for 90 years??
collapse_summary %>% filter(yrs_collapsed > 75)  # Georges Bank Halibut!!


## Exercise 3.1: 
## Try running the same poisson model predicting the number of years that a stock
## is collapsed as a function of the ratio of overfished years and the ratio of
## low stock years. This time, only include data from the US East Coast. Test the 
## poisson model to see if overdispersion is an issue. If it is a problem, refit 
## the model as a quasipoisson. Do you think there is an advantage or a disadvantage
## to breaking the data into distinct regions?

## Okay, so from our zero-truncated tbl, I'm going to filter out the east coast US 
## data. 

unique(collapse_summary_zero_trunc$region)

east_coast_collapse = collapse_summary_zero_trunc %>%
  filter(region=="US East Coast")

## Now I'm going to fit the poisson model.
model_p_us = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_low_stock,
                 offset(log(yrs_data)), data=east_coast_collapse, 
                 family = "poisson")
summary(model_p_us)
## Looks like it's significant in both terms, so the years the stock is overfished 
## AND the years of low stock are both significant drivers of how many years a stock
## is collapsed. 

## Is there overdispersion in our model for the US East Coast?
AER::dispersiontest(model_p_us)$p.value < 0.05
## We received a FALSE, so our data is not overdispersed. Cool cool cool. 
