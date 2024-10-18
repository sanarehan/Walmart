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
## I have the idea to do Friday stock - Monday stock as a column to reflect stock performance for the week

Specific_df <- SalesandEcon_df %>% inner_join(Competition_df)
Specific_df$Store <- as.factor(Specific_df$Store)
Specific_df$Holiday_Flag <- as.factor(Specific_df$Holiday_Flag)

Specific_lm <- lm(Weekly_Sales ~., data=Specific_df)
summary(Specific_lm)


## Random Forest
library(randomForest)
set.seed(123)
Specific_bag <- randomForest(Weekly_Sales ~ .-Store, data = Specific_df,
                        mtry = 3)
plot(Specific_bag)
importance(Specific_bag)
varImpPlot(Specific_bag)
