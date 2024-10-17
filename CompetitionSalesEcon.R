## Merging Competition and Sales and Econ
library(readr)
library(tidyverse)
Competition_df <- read_csv("Competition_df.csv")[ , -1]
SalesandEcon_df <- read_csv("SalesandEcon_df.csv")[ , -1]

Combined_df <- full_join(Competition_df, SalesandEcon_df)

## I just realized that the dates for SalesandEcon are by week. And the Competition dates are by day. Which means I have
## to figure out a way to get a weekly metric for Competition. Should I do the average opening price for that week, or should
## I just do the opening price for that specific day? Maybe I could do both? And see which models better?
## Okay I'll do that another time, I'm good with what I have now