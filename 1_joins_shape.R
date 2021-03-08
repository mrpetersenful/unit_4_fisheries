## ----setup, include=FALSE-----------------------------------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.asp = 0.618, collapse=TRUE) 


## ---- message=FALSE-----------------------------------------------------------------------
library(tidyverse)
data1 <- data.frame(ID = 1:2,                      # Create first example data frame
                    X1 = c("a1", "a2"),
                    stringsAsFactors = FALSE)
data2 <- data.frame(ID = 2:3,                      # Create second example data frame
                    X2 = c("b1", "b2"),
                    stringsAsFactors = FALSE)


## -----------------------------------------------------------------------------------------
# 3 equivalent ways to perform the left_join():

# Without specifying the joining variable:
data12_left = left_join(data1, data2)
# Explicitly specifying the joining variable:
data12_left = left_join(data1, data2, by="ID")
# With piping
data12_left = data1 %>%
  left_join(data2, by="ID")
data12_left



## -----------------------------------------------------------------------------------------
data12_inner = data1 %>%
  inner_join(data2, by="ID")
data12_inner


## -----------------------------------------------------------------------------------------
data12_inner = data1 %>%
  full_join(data2, by="ID")
data12_inner


## -----------------------------------------------------------------------------------------
data12_semi = data1 %>%
  semi_join(data2, by="ID")
data12_semi


## -----------------------------------------------------------------------------------------
data12_anti = data1 %>%
  anti_join(data2, by="ID")
data12_anti


## -----------------------------------------------------------------------------------------
survey = data.frame(quadrat_id = c(101, 102, 103, 104),
                    barnacle_n = c(2, 11, 8, 27),
                    chiton_n = c(1, 0, 0, 2),
                    mussel_n = c(0, 1, 1, 4))


## -----------------------------------------------------------------------------------------
long = survey %>% 
  pivot_longer(c("barnacle_n", "chiton_n", "mussel_n"), names_to="taxon", values_to="counts")
head(long)


## -----------------------------------------------------------------------------------------
wide = long %>%
  pivot_wider(names_from=taxon, values_from=counts)
head(wide)

