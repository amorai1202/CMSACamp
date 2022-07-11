# PURPOSE: Plotting CI's, prediction intervals


# Load data ---------------------------------------------------------------

library(tidyverse)
library(dslabs)

gapminder <- as_tibble(gapminder)
clean_gapminder <- gapminder %>%
  filter(year == 2011, !is.na(gdp)) %>%
  mutate(log_gdp = log(gdp))
init_lm <- lm(life_expectancy ~ log_gdp, data = clean_gapminder)



# Plot confidence interval ------------------------------------------------

lm_plot <- clean_gapminder %>%
  ggplot(aes(x = log_gdp,
             y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "log(GDP)",
       y = "Life expectancy")
lm_plot


# Prediction interval vs CI ----------------------------------------------------

pred_int_data <- 
  predict(init_lm, data = clean_gapminder, 
          interval = "prediction",
          level = .95) %>%
  as_tibble()  
lm_plot +
  geom_ribbon(data =
                bind_cols(clean_gapminder,
                          pred_int_data),
              aes(ymin = lwr, ymax = upr),
              color = "red", fill = NA)


