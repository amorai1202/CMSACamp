# PURPOSE: Brainstorm EDA for MLB Project

# Load the data -----------------------------------------------------------

library(tidyverse)

batted_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/eda_projects/mlb_batted_balls_2022.csv")

# Initial examination of data ---------------------------------------------


Batting <- as_tibble(batted_data)

dim(Batting) #dimensions
class(Batting) #what the object is

head(Batting)
tail(Batting)

summary(Batting)

colnames(Batting)

Batting

length(unique(Batting$player_name)) == nrow(Batting)

head(Batting$bb_type)

# Most common way of getting out -------------------------------------------------------------

table(Batting$events)

Batting_outs <- Batting %>% 
  filter(events != "double", events != "field_error", 
         events != "fielders_choice", events !="home_run", 
         events !="single", events !="triple") %>% 
  mutate(events = 
           fct_recode(events, 
                      "Double Play" = "double_play",
                      "Field Out" = "field_out",
                      "Fielder's Choice Out" = "fielders_choice_out",
                      "Force Out" = "force_out",
                      "Grounded into Double Play" = "grounded_into_double_play",
                      "Sacrifice Bunt" = "sac_bunt",
                      "Sacrifice Fly" = "sac_fly",
                      "Sacrifice Fly Double Play" = "sac_fly_double_play")) 

table(Batting_outs$events)
sort(table(Batting_outs$events), decreasing = TRUE)

Batting_outs$events <- factor(Batting_outs$events, names(sort(table(Batting_outs$events), increasing = TRUE)))

Batting_outs %>% 
  ggplot(aes(x = events)) +
  geom_bar(fill= "#238A8DFF") +
  coord_flip() +
  labs(y= "count", x = "Type of outs", title = "Distribution of ways of getting out") +
  ggthemes::scale_color_colorblind() +
  theme(axis.text = element_text(angle = 90)) +
  theme_bw()
  


# Analyzing change of scores/runs -----------------------------------------

any(is.na(c(Batting$home_score, Batting$post_home_score, 
            Batting$away_score, Batting$post_away_score))) #no NA's

Batting %>% 
  select(home_score, post_home_score, away_score, post_away_score)


Batting_runs <- Batting %>%
  mutate(hometeam_runs = post_home_score - home_score) %>%
  mutate(awayteam_runs = post_away_score - away_score) %>% 
  select(bb_type, hometeam_runs, awayteam_runs) 

# Histogram

## Stacked - marginal

Batting_runs %>%
  ggplot(aes(x = hometeam_runs,
             fill = bb_type)) +
  geom_histogram(bins = 4) +
  theme_bw() + theme(legend.position = "bottom")

## Overlay - conditional

Batting_runs %>%
  ggplot(aes(x = hometeam_runs,
             fill = bb_type)) + 
  geom_histogram(alpha = .25, position = "identity") +
  theme_bw() + theme(legend.position = "bottom")

# OR

Batting_runs %>%
  ggplot(aes(x = hometeam_runs,
             color = bb_type)) + 
  geom_histogram(fill = NA, position = "identity") +
  theme_bw() + theme(legend.position = "bottom")

## Facet Wrap

Batting_runs %>%
  ggplot(aes(x = hometeam_runs)) + 
  geom_histogram() +
  theme_bw() +
  facet_wrap(~ bb_type, ncol = 2, scales = "free_y")

## Facet Grid

Batting_runs %>%
  ggplot(aes(x = hometeam_runs)) + 
  geom_histogram() +
  theme_bw() +
  facet_grid(bb_type ~., margins = TRUE)





# Stance vs hit distance --------------------------------------------------

#Which handed batters hit farther?

Batting %>% 
  ggplot(aes(x = hit_distance_sc,
             fill = stand)) +
  geom_histogram(position = "fill") +
  ggthemes::scale_color_colorblind() +
  theme_bw()


Batting %>% 
  ggplot(aes(x = hit_distance_sc, y = stand)) +
  geom_violin() +
  geom_boxplot(width = .2) +
  theme_bw()

#get sample means of L and R
#2 sample t test



library(ggbeeswarm)
Batting %>% 
  ggplot(aes(y = hit_distance_sc)) +
  geom_beeswarm(aes(x = "")) +
  theme_bw()


ggtitle("Distribution of hit distance by batting stance") +
  ylab = ""
theme_bw()



# L vs R and events
##filter events

# Change labels for events

table(Batting$events)

filter_Batting <- Batting %>% 
  filter(!is.na(events)) %>% 
  mutate(events = 
           fct_recode(events, 
                      "On-base" = "double",
                      "On-base" = "field_error",
                      "Out" = "double_play",
                      "Out" = "field_out",
                      "On-base" = "fielders_choice",
                      "Out" = "fielders_choice_out",
                      "Homerun" = "home_run",
                      "Out" = "force_out",
                      "Out" = "grounded_into_double_play",
                      "Out" = "sac_bunt",
                      "Out" = "sac_fly",
                      "Out" = "sac_fly_double_play",
                      "On-base" = "single",
                      "On-base" = "triple"))

table(filter_Batting$events)

proportions(table(filter_Batting$stand, filter_Batting$events))


table("Stance" = filter_Batting$stand, 
      "Events" = filter_Batting$events) %>% 
  chisq.test()


length(which(filter_Batting$stand=="L"))
length(which(filter_Batting$stand=="R"))

###--------Initial plots

#1 

filter_Batting %>% 
  ggplot(aes(x = stand,
             fill = events)) +
  geom_bar(position = "fill") +
  theme_bw()

#2

filter_Batting %>% 
  ggplot(aes(x = stand,y = 
             fill = events)) +
  geom_bar(position = "dodge") +
  theme_bw()








