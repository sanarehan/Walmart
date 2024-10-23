library(readr)
library(ggplot2)
library(tidyverse)
Merged_df <- read_csv("Merged_df.csv")[,-1]
Merged_df$Store <- as.factor(Merged_df$Store)
Merged_df$Holiday_Flag <- as.factor(Merged_df$Holiday_Flag)

## Exploratory Data Analysis
Merged_df %>%
  ggplot()+
  geom_histogram(aes(x = Weekly_Sales))

Merged_df %>%
  group_by(Holiday_Flag) %>%
  summarize(
    mean = mean(Weekly_Sales),
    median = median(Weekly_Sales),
    stdv = sd(Weekly_Sales),
    max = max(Weekly_Sales),
    min = min(Weekly_Sales)
  )

Merged_df %>%
  ggplot()+
  geom_boxplot(aes(y = Weekly_Sales)) +
  facet_wrap(vars(Holiday_Flag))

hist(Merged_df$Fuel_Price)
hist(Merged_df$CPI)
hist(Merged_df$Unemployment)

plot(Merged_df$Temperature, Merged_df$Weekly_Sales, xlab="Temperature", ylab="Weekly Sales", main="Sales vs Temperature")
plot(Merged_df$Unemployment, Merged_df$Weekly_Sales, xlab="Unemployment", ylab="Weekly Sales", main="Sales vs Unemployment")

library(corrplot)

Numeric_df <- Merged_df[, sapply(Merged_df, is.numeric)]
correlation_matrix <- cor(Numeric_df)
corrplot(correlation_matrix, method = "circle")

plot(Merged_df$COST, Merged_df$Labor_Force)

plot(Merged_df$Date, Merged_df$Weekly_Sales, type="l", main="Weekly Sales Over Time")

Merged_df$Month <- format(as.Date(Merged_df$Date), "%m")
plot(aggregate(Weekly_Sales ~ Month, data = Merged_df, FUN = mean))

plot(aggregate(Weekly_Sales ~ Date, data = Merged_df, FUN = mean))


Holiday_df <- Merged_df %>%
  filter(Holiday_Flag == 1)

Holiday_df %>%
  group_by(Date) %>%
  summarize(
    mean = mean(Weekly_Sales)
  )

Holiday_df %>%
  ggplot()+
  geom_boxplot(
    aes(y = Weekly_Sales)
  ) +
  facet_wrap(vars(Month))



## Multiple Regression

nodiff_lm <- lm(Weekly_Sales ~.-AMZNdiff -COSTdiff -Store, data=Merged_df)
summary(nodiff_lm)

diff_lm <- lm(Weekly_Sales~.-AMZN -COST -Store, data=Merged_df)
summary(diff_lm)


## Random Forest
library(randomForest)
set.seed(123)
Specific_bag <- randomForest(Weekly_Sales ~ .-Store, data = Specific_df,
                             mtry = 3)
plot(Specific_bag)
importance(Specific_bag)
varImpPlot(Specific_bag)

var_imp <- tibble(m = 1:9) %>% 
  group_by(m) %>% 
  group_modify(function (x, gr) {
    set.seed(123)
    rf_m <- randomForest(Weekly_Sales ~ ., data = Specific_df,
                         importance = TRUE,
                         mtry = gr$m)
    as.data.frame(importance(rf_m)) %>% 
      rownames_to_column('predictor')
  })

var_imp %>% 
  pivot_longer(c(IncNodePurity, `%IncMSE`),
               names_to = 'measure',
               values_to = 'value') %>% 
  ggplot(aes(x = m, y = value, color = predictor)) +
  geom_line() +
  geom_point(size = 1.5) +
  scale_color_brewer(type = 'qual', palette = 2) +
  facet_grid(rows = vars(measure), scale = 'free')