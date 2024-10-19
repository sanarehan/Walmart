## Merging Competition and Sales and Econ
library(readr)
library(tidyverse)
library(dplyr)
Competition_df <- read_csv("Competition_df.csv")[ , -1]
SalesandEcon_df <- read_csv("SalesandEcon_df.csv")[ , -1] 

SalesandEcon_df$Store = as.factor(SalesandEcon_df$Store)
SalesandEcon_df$Holiday_Flag = as.factor(SalesandEcon_df$Holiday_Flag)


Combined_df <- inner_join(Competition_df, SalesandEcon_df)

## I have the idea to do last day stock - first day stock as a column to reflect stock performance for the week

Dates_tibble <- Combined_df %>%
  filter(Store == "1") %>%
  select(AMZN, COST, Date) %>%
  mutate(
    AMZNdiff = AMZN - lag(AMZN),
    COSTdiff = COST - lag(COST)
  )

Dates_tibble$AMZNdiff[1] <- Competition_df$AMZN[30]-Dates_tibble$AMZN[1]
Dates_tibble$COSTdiff[1] <- Competition_df$COST[30]-Dates_tibble$COST[1]

Merged_df <- SalesandEcon_df %>% inner_join(Dates_tibble, by="Date")

write.csv(Merged_df, file="Merged_df.csv")
