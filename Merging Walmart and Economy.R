library(readr)
Walmart_Sales <- read_csv("C:/Users/sanar/Downloads/archive (4)/Walmart_Sales.csv")
CIVPART <- read_csv("C:/Users/sanar/Downloads/CIVPART.csv")

library(tidyverse)
library(dplyr)

# I want to get the data just from 2010-2012
CIVPART_1012 <- CIVPART %>% separate(
  col = DATE,
  sep = "-",
  into = c("YEAR", "MONTH", "DAY")
) %>%
  filter(
    YEAR == 2010 | YEAR == 2011 | YEAR == 2012
  ) %>%
  mutate(
    MONTHYEAR = paste(MONTH, YEAR, sep="-")
  ) %>%
  select(
    -DAY, -MONTH, -YEAR)

Walmart_MONTHYEAR <- Walmart_Sales %>%
  separate(
    col = Date,
    sep = "-", 
    into = c("DAY", "MONTH", "YEAR")
  ) %>%
  mutate(
    MONTHYEAR = paste(MONTH, YEAR, sep="-")
  )

SalesandEcon_df <- inner_join(CIVPART_1012, Walmart_MONTHYEAR, by="MONTHYEAR") %>%
  mutate(
    Date = paste(YEAR, MONTH, DAY, sep="-")
  ) %>% 
  select(-MONTHYEAR, -DAY, -MONTH, -YEAR) %>%
  dplyr::rename(
    Labor_Force = CIVPART
  )

## Now I can output SalesandEcon_df

write.csv(SalesandEcon_df, file="SalesandEcon_df.csv")
