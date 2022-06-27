# PURPOSE: Initial regression models of life expectancy


# Load data ---------------------------------------------------------------

library(tidyverse)
library(dslabs)
gapminder <- as_tibble(gapminder)
clean_gapminder <- gapminder %>%
  filter(year == 2011, !is.na(gdp)) %>% #subset of data
  mutate(log_gdp = log(gdp)) #created column of log gdp 
clean_gapminder

#distribution of y DOES NOT have to look like a bell curve

clean_gapminder %>%
  ggplot(aes(x = life_expectancy)) +
  geom_histogram(color = "black", 
                 fill = "darkblue",
                 alpha = 0.3) +
  theme_bw() +
  labs(x = "Life expectancy")

#for a fixed x, does y look like a bell curve?


# Model relationship between gdp and life expectancy ----------------------

#View plots of relationship

gdp_plot <- clean_gapminder %>%
  ggplot(aes(x = log_gdp,
             y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  labs(x = "log(GDP)",
       y = "Life expectancy")
gdp_plot

init_lm <- lm(life_expectancy ~ log_gdp, #lm(reponse ~ explanatory, data = )
              data = clean_gapminder)

summary(init_lm)


# Multiple r-squared ------------------------------------------------------

with(clean_gapminder, cor(log_gdp, life_expectancy))

with(clean_gapminder, cor(log_gdp, life_expectancy))^2
#OR
var(predict(init_lm)) / var(clean_gapminder$life_expectancy)
#changes in predicted values / variance in Y


# Play around with predictions --------------------------------------------

train_preds <- predict(init_lm)
head(train_preds)
#OR
head(init_lm$fitted.values)

# Predictions for new data

us_data <- clean_gapminder %>% 
  filter(country == "United States")

new_us_data <- us_data %>%
  dplyr::select(country, gdp) %>%
  slice(rep(1, 3)) %>% #copy of dataset 3 times
  mutate(adj_factor = c(0.25, 0.5, 0.75),
         log_gdp = log(gdp * adj_factor))

new_us_data$pred_life_exp <- #add new column
  predict(init_lm, newdata = new_us_data)

gdp_plot + # add predictions on plot
  geom_point(data = new_us_data,
             aes(x = log_gdp,
                 y = pred_life_exp),
             color = "darkred", size = 5, 
             alpha = 0.8)

# Plot observed values against predictions (diagnostic)

gdp_plot +
  geom_smooth(method = "lm") #best fit line

#If grey error bands are thick enough so that you can tilt the line to be horizontal, evidence suggests relationship

gdp_plot +
  geom_smooth(method = "lm", se = FALSE) #remove error bands

#OR
gdp_plot + #explicitly putting in line with slope and intercept
  geom_smooth(method = "lm") +
  geom_abline(slope = init_lm$coefficients[2],
              intercept = init_lm$coefficients[1],
              color = "red")


clean_gapminder %>%
  mutate(pred_vals = predict(init_lm)) %>%
  ggplot(aes(x = pred_vals,
             y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, #add reference line (perfect diagonal line)
              linetype = "dashed",
              color = "red",
              size = 2) +
  theme_bw()

#if predictions matched our observed data perfectly, they will fall perfectly on the line

clean_gapminder <- 
  broom::augment(init_lm, clean_gapminder) #using broom package

clean_gapminder %>%
  ggplot(aes(x = .fitted, 
             y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed",
              color = "red",
              size = 2) +
  theme_bw()

# Residual diagnostic plot

clean_gapminder %>%
  ggplot(aes(x = .fitted, 
             y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red",
             size = 2) +
  # To plot the residual mean/trend
  geom_smooth(se = FALSE) +
  theme_bw()





