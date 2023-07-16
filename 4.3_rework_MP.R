### Lesson 4.3: Generalized linear models (GLMs)
## New skills: glm(family="binomial"), glm(family="poisson"), AER::dispersiontest, glm(family="quasipoisson")


## Exploring fisheries data with GLMs

# In this lesson we will continue to use the RAM Legacy Database:

# https://www.ramlegacy.org/

load('data/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData')

# First I'm going to import the code for the tables we built in the last class, such as fish_catch_max_assess and collapse. That way I can use the data manipulation work we've already done as a basis for running our models today. I just put all of the code used to generate these tables in their own R script, and now I use the source() function to run that R script and put all of the associated variables in my environment. You could also just copy and paste the code we wrote in the last class at the top of today's R script. 

source('build_collapse_table.R')


## Logistic regression

# Logistic regression is used to model a binary dependent variable or a dependent variable that is bound between 0 and 1.

# This is the logistic regression function where p(x) is our y variable, bounded between 0 and 1: 

# p(x) = (exp(beta0 + beta1*X))/(1 + exp(beta0 + beta1*X))

# Although this equation doesn't look like a linear equation, a logistic function is a generalized linear model because the model is linear in the predictors, meaning that the model is to fit some linear combination of the x variables (the independent variables) and the model coefficients. That means that the equation can be re-arranged with x's all on one side of the equation looking like an ordinary linear regression model: 

# log(p(x)/(1-p(x))) = beta0 + beta1*X

# Logistic regression can be fit with the glm() function using the binomial family. glm stands for Generalized Linear Models, and binomial implies that the y variable that we are predicting has 2 choices. After the model is fit, model predictions can be made with the predict() function just like we did with linear regressions. 

# Logistic regression does not require model residuals to be normally distributed or variance to be constant. AIC can be used for model comparison, i.e. to determine whether the inclusion of an additional independent variable constitutes a significantly improved model. McFadden's Pseudo-R^2 can be used to assess fitness, where McFadden claims that a Pseudo-R^2 between 0.2 and 0.4 represents an "excellent fit". 


## Which stocks have experienced a collapse? 

# What features of a stock make it more (or less) likely to experience a collapse? I find this question fascinating, and I'd love to model the chance of collapse as a function of habitat type, geographic region, primary country that fishes that stock, fishing methods and age at sexual maturity. Unfortunately, the RAM data doesn't have a lot of that background info for most stocks (see bioparams table). We do consistently have region, i.e. the geographical location of the stock, so let's see if region is a significant explanatory variable for predicting the stock's collapse. (Note that a chi-square test would also be an appropriate way to answer this question since we are comparing a categorican y variable against a single categorical x variable, but if we use a logistic regression model we could add more complexity with multiple x variables). 

# Since collapse is a yes/no, TRUE/FALSE, 0/1, binomial type of variable, we should use logistic regression. 

# Remove time series information and collapse data to "has this stock EVER collapsed?"
model_data = collapse %>%
  group_by(stockid) %>% 
  summarize(ever_collapsed = any(current_collapse)) %>%
  ungroup() %>% left_join(stock)
head(model_data)

summary(model_data)

# Run a logistic regression
model_l = glm(ever_collapsed ~ region, data = model_data, family = "binomial")
summary(model_l)

model_data %>% distinct(region) %>% arrange(region)

# We have statistically significant, negative coefficients for the Mediterranean-Black Sea stocks and the West Africa stocks - meaning these stocks are less likely to collapse than the reference region, which is the Atlantic Ocean. There are no examples of collapsed stocks in the Pacific Ocean and Indian Ocean regions. 

# To create the prediction plot, we use predict.glm and generate model predictions of a stock's probability of ever experiencing a collapse for every region in the dataset. We generate predictions with type="response" because we want our predictions to be on the scale of the response (y) variable. That means we want predictions between 0 and 1, using the p(x) = ... equation listed above. 

# It is important to note that the default option for predict.glm is to generate predictions with type="link", meaning that the predictions are generated on the scale of the linear predictors. In the case of a logistic regression, these predictions would be log-odds, and are calculated using the log() eq'n listed above. 

# Since I want to plot the probability that a stock might experience a collapse for a given region, I'll use type="response".

# Make predictions on the probability of a stock collapse by region 
regions = model_data %>% distinct(region)
model_l_predict = predict(model_l, newdata=regions, type="response", se.fit=TRUE)

# Organize predictions into a tidy table
collapse_region_predictions = cbind(regions, model_l_predict)

# Plot predictions and SE bars
dev.new()
ggplot(aes(x=region, y=fit, fill=region), data=collapse_region_predictions) +
  geom_bar(stat="identity", show.legend = FALSE) + 
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=0.2) + 
  coord_flip() + 
  ylab("Probability of stock collapse")

# Calculate McFadden Pseudo-R^2: 0.2-0.4 = "Excellent fit"
install.packages("pscl")
library(pscl)
pscl::pR2(model_l)["McFadden"] # Returns 0.1231096

# Logistic regression can be a good tool for any type of binomial analysis, including binomial categorical variables (like male/female, pass/fail, yes/no), probabilities (including demographic or state space transition probabilities), or presence/absence occurrence data. Use it. Love it. 


## Poisson model


