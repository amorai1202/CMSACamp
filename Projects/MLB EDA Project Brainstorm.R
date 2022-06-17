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









# Most popular pitch type depending on count ------------------------------

# Overall pitch types
edit_pitch <- Batting %>% 
  filter(!is.na(pitch_type)) %>% 
  mutate(pitch_type = fct_recode(pitch_type, "Changeup" = "CH", 
                                 "Breaking ball" = "CU",
                                 "Changeup" = "EP",
                                 "Fastball" = "FA",
                                 "Fastball" = "FC", 
                                 "Fastball" = "FF", 
                                 "Fastball" = "FS",
                                 "Breaking ball" = "KC",  
                                 "Fastball" = "SI",  
                                 "Breaking ball" = "SL"))

# 0-0
zero_zero <- edit_pitch %>% 
  filter(balls == 0) %>% 
  filter(strikes == 0) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "zero_zero")


# 0-1
zero_one <- edit_pitch %>% 
  filter(balls == 0) %>% 
  filter(strikes == 1) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "zero_one")

# 0-2
zero_two <- edit_pitch %>% 
  filter(balls == 0) %>% 
  filter(strikes == 2) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "zero_two")

# 1-0
one_zero <- edit_pitch %>% 
  filter(balls == 1) %>% 
  filter(strikes == 0) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "one_zero")

# 1-1
one_one <- edit_pitch %>% 
  filter(balls == 1) %>% 
  filter(strikes == 1) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "one_one")

# 1-2
one_two <- edit_pitch %>% 
  filter(balls == 1) %>% 
  filter(strikes == 2) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "one_two")

# 2-0
two_zero <- edit_pitch %>% 
  filter(balls == 2) %>% 
  filter(strikes == 0) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "two_zero")


# 2-1
two_one <- edit_pitch %>% 
  filter(balls == 2) %>% 
  filter(strikes == 1) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "two_one")

# 2-2
two_two <- edit_pitch %>% 
  filter(balls == 2) %>% 
  filter(strikes == 2) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "two_two")

# 3-0
three_zero <- edit_pitch %>% 
  filter(balls == 3) %>% 
  filter(strikes == 0) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "three_zero")

# 3-1
three_one <- edit_pitch %>% 
  filter(balls == 3) %>% 
  filter(strikes == 1) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "three_one")

# 3-2
three_two <- edit_pitch %>% 
  filter(balls == 3) %>% 
  filter(strikes == 2) %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  mutate(prop = n / sum(n), count = "three_two")

prop_count <- zero_zero %>% 
  rbind(zero_one, zero_two, one_zero, one_one, one_two, two_one, two_two, 
        two_zero, three_zero, three_one, three_two)

prop_count %>% 
  ggplot(aes(x = pitch_type, y = prop, fill = count)) +
  geom_col(position = "dodge") +
  theme_bw() 
 


# Initial clustering ------------------------------------------------------

## player name and hit distance, launch speed

# Look at data/ remove na's

Batting <- Batting %>% 
  filter(!is.na(hit_distance_sc), !is.na(launch_speed)) 

# Standardize first

std_batting <- Batting %>%
  mutate(std_hit = as.numeric(scale(hit_distance_sc)),
         std_speed = as.numeric(scale(launch_speed)))

std_batting %>%
  ggplot(aes(x = std_hit, y = std_speed)) +
  geom_point(alpha = 0.5) + 
  theme_bw() +
  coord_fixed()

# Try k-means clustering

## elbow plot

# Initialize number of clusters to search over
n_clusters_search <- 2:12
tibble(total_wss = 
         # Compute total WSS for each number by looping with sapply
         sapply(n_clusters_search,
                function(k) {
                  kmeans_results <- kmeans(dplyr::select(std_batting,
                                                         std_hit,
                                                         std_speed),
                                           centers = k, nstart = 30)
                  # Return the total WSS for choice of k
                  return(kmeans_results$tot.withinss)
                })) %>%
  mutate(k = n_clusters_search) %>%
  ggplot(aes(x = k, y = total_wss)) +
  geom_line() + geom_point() +
  labs(x = "Number of clusters K", y = "Total WSS") +
  theme_bw()

## k-means

init_kmeans <- 
  kmeans(dplyr::select(std_batting,
                       std_hit, std_speed),
         algorithm = "Lloyd", centers = 3,
         nstart = 30)

std_batting %>%
  mutate(batting_clusters = 
           as.factor(init_kmeans$cluster)) %>%
  ggplot(aes(x = std_hit, y = std_speed,
             color = batting_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_fixed()

# k-means++

library(flexclust)

init_kmeanspp <- 
  kcca(dplyr::select(std_batting,
                     std_hit, std_speed), k = 3,
       control = list(initcent = "kmeanspp"))
std_batting %>%
  mutate(batting_clusters = 
           as.factor(init_kmeans$cluster)) %>%
  ggplot(aes(x = std_hit, y = std_speed,
             color = batting_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_fixed()


# Try hierarchical clustering

#minimax

library(protoclust)

batted_dist <- dist(dplyr::select(std_batting,
                                  std_hit, std_speed))

batted_minimax <- protoclust(batted_dist)

# Look at dendrogram
library(ggdendro)
ggdendrogram(batted_minimax, 
             theme_dendro = FALSE, 
             labels = FALSE, 
             leaf_labels = FALSE) + 
  labs(y = "Maximum dissimilarity from prototype") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())


minimax_batted_clusters <- 
  protocut(batted_minimax, k = 3) #where to cut tree

std_batting %>%
  mutate(batted_clusters = 
           as.factor(minimax_batted_clusters$cl)) %>%
  ggplot(aes(x = std_hit, y = std_speed,
             color = batted_clusters)) +
  geom_point(alpha = 0.3) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")



#single

batted_dist <- dist(dplyr::select(std_batting,
                                  std_hit, std_speed))

batted_single_hclust <-
  hclust(batted_dist, method = "single")


std_batting %>%
  mutate(batted_clusters = 
           as.factor(cutree(batted_complete_hclust, k=3))) %>% #cut dendrogram using chosen height h
  ggplot(aes(x = std_hit, y = std_speed,
             color = batted_clusters)) +
  geom_point(alpha = 0.5) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

#complete

batted_complete_hclust <-
  hclust(batted_dist, method = "complete")

std_batting %>%
  mutate(batted_clusters = 
           as.factor(cutree(batted_complete_hclust, k=3))) %>% #cut dendrogram using chosen height h
  ggplot(aes(x = std_hit, y = std_speed,
             color = batted_clusters)) +
  geom_point(alpha = 0.5) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")



# Summarize batted balls by player name -----------------------------------

#mean(table(Batting$player_name)) 
#average at bats = 17, remove players who've batted less than 5 times 

players <- Batting %>% 
  group_by(player_name) %>% 
  filter(n() > 5) %>%  #filter players who've batted more than 5 times
  summarise_all(.funs = mean, .groups = "drop") #take player averages

filter_players <- players %>% 
  drop_na(hit_distance_sc, launch_speed) #get rid of rows that have NA's in hit distance and launch speed


# Try hierarchical clustering
#minimax

library(protoclust)
player_dist <- dist(dplyr::select(filter_players,
                                  hit_distance_sc, launch_speed))
player_minimax <- protoclust(player_dist)

# Look at dendrogram
library(ggdendro)
ggdendrogram(player_minimax, 
             theme_dendro = FALSE, 
             labels = FALSE, 
             leaf_labels = FALSE) + 
  labs(y = "Maximum dissimilarity from prototype") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())


minimax_player_clusters <- 
  protocut(player_minimax, k = 3) #where to cut tree

filter_players %>%
  mutate(player_clusters = 
           as.factor(minimax_player_clusters$cl)) %>%
  ggplot(aes(x = hit_distance_sc, y = launch_speed,
             color = player_clusters)) +
  geom_point(alpha = 0.5) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

# View who the prototypes are
player_prototypes <- filter_players %>%
  dplyr::select(player_name, hit_distance_sc, launch_speed) %>%
  slice(minimax_player_clusters$protos)

# Label prototypes on graph 
filter_players %>%
  mutate(player_clusters = 
           as.factor(minimax_player_clusters$cl)) %>%
  ggplot(aes(x = hit_distance_sc, y = launch_speed,
             color = player_clusters)) +
  geom_point(alpha = 0.5) + 
  geom_label(data = mutate(player_prototypes, 
                           player_clusters = 
                             as.factor(c(1,2,3))), aes(label = player_name)) +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

# View cluster 3

best_batters <- filter_players %>%
  mutate(player_clusters = 
           as.factor(minimax_player_clusters$cl)) %>% 
  filter(player_clusters == 3) %>% 
  pull(player_name)

data.frame(best_batters) #group of strongest batters this season






