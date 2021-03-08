## ----setup, include=FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.asp = 0.618, collapse=TRUE) 


## -----------------------------------------------------------------------------------------
load('data/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData')


## ---- results="hide", message=FALSE-------------------------------------------------------
source('build_collapse_table.R')


## -----------------------------------------------------------------------------------------
# Remove time series information and collapse data to "has this stock EVER collapsed?"
model_data = collapse %>%
  group_by(stockid, region) %>%
  summarize(ever_collapsed = any(ever_collapsed)) %>%
  ungroup()
glimpse(model_data)

# Run a logistic regression
model_l = glm(ever_collapsed ~ region, data = model_data, family = "binomial")
summary(model_l)

# Make predictions on the probability of a stock collapse by region
regions = model_data %>% distinct(region)
model_l_predict = predict(model_l, newdata=regions, type="response", se.fit=TRUE)

# Organize predictions into a tidy table
collapse_region_predictions = data.frame(region = regions,
                                         predictions = model_l_predict$fit,
                                         se = model_l_predict$se.fit)

# Plot predictions and SE bars
ggplot(aes(x=region, y=predictions, fill=region), data=collapse_region_predictions) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=predictions-se, ymax=predictions+se), width=0.2) +
  coord_flip() +
  theme(legend.position = "none")

# Calculate McFadden Pseudo-R^2: 0.2-0.4 = "Excellent fit"
# library(pscl)
# pscl::pR2(model_l)["McFadden"]


## -----------------------------------------------------------------------------------------
# U is fishing pressure (often fishing mortalities) and B is biomass
# U / U_MSY and B / B_MSY?
# Calculate ratio of years a stock is overfished and stock biomass < B_MSY
u_summary = timeseries_values_views %>% 
  left_join(stock, by=c("stockid","stocklong")) %>%
  filter(!is.na(UdivUmsypref),
         !is.na(BdivBmsypref)) %>%
  group_by(stockid) %>%
  summarize(yrs_data = n(),
            ratio_yrs_overfished = sum(UdivUmsypref > 1)/yrs_data,
            ratio_yrs_low_stock = sum(BdivBmsypref < 1)/yrs_data) %>%
  select(-yrs_data)

# Count num years each stock is collapsed; join with counts of overfished-years and low-stock-years
collapse_summary = collapse %>%
  group_by(stockid, stocklong, region) %>%
  summarize(yrs_data = n(),
            yrs_collapsed = sum(current_collapse)) %>%
  left_join(u_summary, by="stockid")

# Do we have zero-inflation?
table(collapse_summary$yrs_collapsed)

# Create a zero-truncated data set to demonstrate poisson model
collapse_summary_zero_trunc = collapse_summary %>% filter(yrs_collapsed>0)

# Build poisson model
model_p = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_low_stock, offset(log(yrs_data)), data=collapse_summary_zero_trunc, family="poisson") 
summary(model_p)

# Do we have overdispersion?
AER::dispersiontest(model_p)$p.value < 0.05 # TRUE = overdispersed; FALSE = NOT overdispersed

# Address overdispersion with a quasipoisson or negative binomial model
model_qp = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_low_stock , offset(log(yrs_data)), data=collapse_summary_zero_trunc, family="quasipoisson") 
summary(model_qp)

# Make predictions on the time period a stock spends collapsed 
# as a function of low stock year rates with overfishing rates set to the observed median
newdata = data.frame(ratio_yrs_low_stock = seq(from=0,to=1,by=0.1),
                     ratio_yrs_overfished = median(collapse_summary_zero_trunc$ratio_yrs_overfished, na.rm=TRUE))
model_qp_predict = predict(model_qp, type="response", newdata = newdata, se.fit=TRUE)

# Organize predictions into a tidy table
collapse_time_predictions = cbind(newdata,
                                  data.frame(predictions = model_qp_predict$fit,
                                             se = model_qp_predict$se.fit))

# Plot predictions and SE ribbon
ggplot() +
  geom_line(aes(x=ratio_yrs_low_stock, y=predictions), data=collapse_time_predictions) +
  geom_ribbon( aes(x=ratio_yrs_low_stock, ymin = predictions-se, ymax = predictions+se), fill="darkgrey", alpha = .5, data=collapse_time_predictions) +
  geom_point(aes(x=ratio_yrs_low_stock, y=yrs_collapsed), data=collapse_summary_zero_trunc) +
  theme_bw()

# What fishery has been collapsed for 90 years??
collapse_summary %>% filter(yrs_collapsed > 75)  # Georges Bank Halibut!!


