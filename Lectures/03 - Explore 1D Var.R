# PURPOSE: Exporing 1D categorical and continuous variables


# Load the data -----------------------------------------------------------

library(tidyverse)
ohtani_batted_balls <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/xy_examples/ohtani_2021_batted_balls.csv")
head(ohtani_batted_balls) #each row is a batted ball in 2021 season by Ohtani


# Visualize 1D Categorical Variables using Barcharts ---------------------
##to display frequencies of outcomes


# Make a bar chart of batted ball type 

ohtani_batted_balls %>%
  ggplot(aes(x = batted_ball_type)) +
  geom_bar() + #counts values  (transformation) in data set behind the scenes
  theme_bw()

## Shows the marginal distribution


# Proportions plot 

ohtani_batted_balls %>%
  ggplot(aes(x = batted_ball_type)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) + #proportions
  theme_bw()

# Manual

ohtani_batted_balls %>%
  group_by(batted_ball_type) %>%
  summarize(count = n(), .groups = "drop") %>%
  #ungroup() %>%
  mutate(total = sum(count),
         prop = count / total) %>%
  ggplot(aes(x = batted_ball_type)) +
  geom_bar(aes(y = prop),
           stat = "identity") +
  theme_bw()

# Add error bars

ohtani_batted_balls %>%
  group_by(batted_ball_type) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(total = sum(count),
         prop = count / total,
         se = sqrt(prop * (1 - prop) / total),
         lower = prop - 2 * se,
         upper = prop + 2 * se) %>%
  ggplot(aes(x = batted_ball_type)) +
  geom_bar(aes(y = prop),
           stat = "identity") + 
  geom_errorbar(aes(ymin = lower,
                    ymax = upper),
                color = "red") +
  theme_bw()

# Order visually using factor (fct_reorder)

ohtani_batted_balls %>%
  group_by(batted_ball_type) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(total = sum(count),
         prop = count / total,
         se = sqrt(prop * (1 - prop) / total),  
         lower = prop - 2 * se, 
         upper = prop + 2 * se,
         batted_ball_type =
           fct_reorder(batted_ball_type,
                       prop)) %>%
  ggplot(aes(x = batted_ball_type)) +
  geom_bar(aes(y = prop),
           stat = "identity") + 
  geom_errorbar(aes(ymin = lower, 
                    ymax = upper),
                color = "red") + 
  theme_bw()



# Visualize cat variable - exit velocity ----------------------------------

summary(ohtani_batted_balls$exit_velocity)

# Using boxplots (for summary statistics)

ohtani_batted_balls %>%
  ggplot(aes(y = exit_velocity)) +
  geom_boxplot(aes(x = "")) +
  theme_bw() +
  coord_flip()

# Using histograms (for individual observations)

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity)) +
  geom_histogram(bins = 30) +
  theme_bw()


# Using beeswarm plots (for small datasets)

library(ggbeeswarm)
ohtani_batted_balls %>%
  ggplot(aes(y = exit_velocity)) + 
  geom_beeswarm(aes(x = ""),
                cex = 3) +
  theme_bw() +
  coord_flip()


# Using violin plots 

ohtani_batted_balls %>%
  ggplot(aes(y = exit_velocity)) + 
  geom_violin(aes(x = "")) +
  theme_bw() +
  coord_flip()

# Add summary statistics to violin using boxplot

ohtani_batted_balls %>%
  ggplot(aes(y = exit_velocity,
             x = "")) +   #2 plots share aes
  geom_violin() + 
  geom_boxplot(width = .2) +
  theme_bw() +
  coord_flip()


# Display full distribution with ECDF plot --------------------------------

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity)) + 
  stat_ecdf() +
  theme_bw()

# Add rug plots

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity)) +
  geom_rug(alpha = 0.3) +
  stat_ecdf() +
  theme_bw()


# Scatterplot for 2D continuous data --------------------------------------

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity,
             y = launch_angle)) +
  geom_point(alpha = 0.4) + #ALWAYS CHANGE TRANSPARENCY
  geom_rug(alpha = 0.4) +
  theme_bw()







