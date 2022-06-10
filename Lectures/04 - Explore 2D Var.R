# PURPOSE: Create 2D categorical visualization with some inference


# Load data ---------------------------------------------------------------

library(tidyverse)
ohtani_batted_balls <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/xy_examples/ohtani_2021_batted_balls.csv")
head(ohtani_batted_balls)


# Explore pitch type variable ---------------------------------------------

table(ohtani_batted_balls$pitch_type)

## any(is.na(ohtani_batted_balls$pitch_type)) check if any missing data


# Change labels

ohtani_batted_balls <- ohtani_batted_balls %>% 
  filter(!is.na(pitch_type)) %>% 
  mutate(pitch_type = 
           fct_recode(pitch_type, 
                      "Changeup" = "CH",
                      "Breaking ball" = "CU",
                      "Fastball" = "FC",
                      "Fastball" = "FF",
                      "Fastball" = "FS",
                      "Breaking ball" = "KC",
                      "Fastball" = "SI",
                      "Breaking ball" = "SL"))
  
table(ohtani_batted_balls$pitch_type)

# Chi-Squared Test

chisq.test(table(ohtani_batted_balls$pitch_type))


# 2D viz with pitch type and batted ball type -----------------------------

# Stacked/spine -- MARGINAL 

ohtani_batted_balls %>%
  ggplot(aes(x = batted_ball_type,
             fill = pitch_type)) +
  geom_bar() + 
  ggthemes::scale_fill_colorblind() +
  theme_bw()

# Side-by-Side -- CONDITIONAL

ohtani_batted_balls %>% 
  ggplot(aes(x = batted_ball_type,
             fill = pitch_type)) +
  geom_bar(position = "dodge") +
  ggthemes::scale_color_colorblind() +
  theme_bw()


# Contingency table -------------------------------------------------------

table("Pitch type" = ohtani_batted_balls$pitch_type, 
      "Batted ball type" = ohtani_batted_balls$batted_ball_type)

proportions(table(ohtani_batted_balls$pitch_type, ohtani_batted_balls$batted_ball_type))

#OR 

table("Pitch type" = ohtani_batted_balls$pitch_type, 
      "Batted ball type" = ohtani_batted_balls$batted_ball_type) %>% 
  proportions()

#Reformat table

library(gt)
ohtani_batted_balls %>%
  group_by(batted_ball_type, pitch_type) %>%
  summarize(joint_prob = n() / nrow(ohtani_batted_balls)) %>%
  pivot_wider(names_from = batted_ball_type, values_from = joint_prob,
              values_fill = 0) %>%
  gt()


# Chi-squared test

table("Pitch type" = ohtani_batted_balls$pitch_type, 
      "Batted ball type" = ohtani_batted_balls$batted_ball_type) %>% 
  chisq.test()


# Mosaic plot -------------------------------------------------------------

mosaicplot(table(ohtani_batted_balls$pitch_type, ohtani_batted_balls$batted_ball_type),
           main = "Relationship between batted ball and pitch type?")


# Shade using Pearson residuals (shade = TRUE)

mosaicplot(table(ohtani_batted_balls$pitch_type, ohtani_batted_balls$batted_ball_type),
           shade = TRUE, main = "Relationship between batted ball and pitch type?")


# Continuous by Categorical Viz -------------------------------------------

# Box & Violin

ohtani_batted_balls %>%
  ggplot(aes(x = pitch_type,
             y = exit_velocity)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  theme_bw()

# ECDF

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity,
             color = pitch_type)) +
  stat_ecdf() + 
  theme_bw() +
  theme(legend.position = "bottom")

# Histogram

## Stacked - marginal

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity,
             fill = pitch_type)) +
  geom_histogram() +
  theme_bw() + theme(legend.position = "bottom")

## Overlay - conditional

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity,
             fill = pitch_type)) + 
  geom_histogram(alpha = .25, position = "identity") +
  theme_bw() + theme(legend.position = "bottom")

# OR

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity,
             color = pitch_type)) + 
  geom_histogram(fill = NA, position = "identity") +
  theme_bw() + theme(legend.position = "bottom")

## Facet Wrap

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity)) + 
  geom_histogram() +
  theme_bw() +
  facet_wrap(~ pitch_type, ncol = 2, scales = "free_y")

## Facet Grid

ohtani_batted_balls %>%
  ggplot(aes(x = exit_velocity)) + 
  geom_histogram() +
  theme_bw() +
  facet_grid(pitch_type ~., margins = TRUE)


# Facets beyond 2D --------------------------------------------------------

ohtani_batted_balls %>%
  ggplot(aes(x = pitch_type,
             fill = batted_ball_type)) + 
  geom_bar() + theme_bw() +
  facet_wrap(~ outcome, ncol = 5) +
  theme(legend.position = "bottom")



