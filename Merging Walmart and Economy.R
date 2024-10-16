library(readr)
Walmart_Sales <- read_csv("C:/Users/sanar/Downloads/archive (4)/Walmart_Sales.csv")
CIVPART <- read_csv("C:/Users/sanar/Downloads/CIVPART.csv")

library(tidyverse)
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

full_df <- full_join(CIVPART_1012, Walmart_MONTHYEAR) %>%
 mutate(
    Date = paste(DAY, MONTH, YEAR, sep="-")
  ) %>% 
  select(-MONTHYEAR, -DAY, -MONTH, -YEAR) %>%
  rename(
    Labor_Force = CIVPART
  )
  
