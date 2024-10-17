library(readr)
AMZN <- read_csv("C:/Users/sanar/Downloads/AMZN Stocks Data - Sheet1.csv")
COST <- read_csv("C:/Users/sanar/Downloads/Stocks Data - COST Stocks Data.csv")

library(tidyverse)
library(dplyr)
library(plyr)

AMZN_open <- AMZN %>%
  select(Date, Open) %>%
  dplyr::rename(AMZN = Open)

COST_open <- COST %>%
  select(Date, Open) %>%
  dplyr::rename(COST = Open)

open_df <- full_join(AMZN_open, COST_open, by="Date") %>%
  separate(
    col=Date,
    sep=",",
    into = c("Month-Day", "Year")
  ) %>%
  separate(
    col="Month-Day",
    sep=" ",
    into=c("Month", "Day")
  )

## I need to figure out how to add zeros to the month and day
## I had the idea to create a code to map values from 1-9 to 01 etc I guess I'll just do that

num <- c("01", "02", "03", "04", "05", "06", "07", "08", "09")
num_df <- open_df %>% mutate(
    Month_num = plyr::mapvalues(open_df$Month, from=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), to=c(num, 10, 11, 12)),
    Day_num = plyr::mapvalues(open_df$Day, from=(1:9), to=num)
  ) 

Competition_df <- num_df %>%
  mutate(
    Year = statnet.common::despace(Year)
  ) %>%
  mutate(
    Date = paste(Day_num, Month_num, Year, sep="-")
  ) %>%
  select(
    -Month_num, -Day_num, -(Month:Year)
  )

## Now I can export Competition_df

write.csv(Competition_df, file="Competition_df.csv")
