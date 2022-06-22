# PURPOSE: Explore model-based clustering


# Load the data -----------------------------------------------------------

library(tidyverse)
nba_pos_stats <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/clustering/nba_2022_player_per_pos_stats.csv")
tot_players <- nba_pos_stats %>% filter(tm == "TOT")
nba_player_stats <- nba_pos_stats %>% filter(!(player %in% tot_players$player)) %>% 
  bind_rows(tot_players)
nba_filtered_stats <- nba_player_stats %>% filter(mp >= 125)
head(nba_filtered_stats)


# Load mclust package -----------------------------------------------------

library(mclust)


# Fit GMMs ----------------------------------------------------------------

nba_mclust <- Mclust(dplyr::select(nba_filtered_stats, x3pa, trb))
#searching over 1 to 9 clusters (K = G) and the different covariance constraints (i.e. models)

summary(nba_mclust)

plot(nba_mclust) 
#Picked VVI (no tilt - independent variables, but variances differ)

plot(nba_mclust, what = 'BIC', 
     legendArgs = list(x = "bottomright", ncol = 4))
#BIC: highest = best one/optimal num of components

plot(nba_mclust, what = 'classification')
#Classification: Gaussian components (ellipses) drawn on plot

plot(nba_mclust, what = 'uncertainty')
#Uncertainty: Size reflects the uncertainty (mostly on the border between clusters)

nba_mclust$parameters$mean #means for each component
nba_mclust$parameters$variance$sigma #3 covariance matrices

table("Clusters" = nba_mclust$classification, 
      "Positions" = nba_filtered_stats$pos) #compare the clustering assignments with player positions

#manual plot of clustering
nba_filtered_stats %>% 
  mutate(gmm_class = as.factor(nba_mclust$classification)) %>% 
  ggplot(aes(x = x3pa, y = trb, color = gmm_class)) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "bottom")

#manually adding uncertainty
nba_filtered_stats %>% 
  mutate(gmm_class = as.factor(nba_mclust$classification),
         gmm_uncertainty = nba_mclust$uncertainty) %>% 
  ggplot(aes(x = x3pa, y = trb, color = gmm_class)) +
  geom_point(alpha = 0.5) +
  guides(size = FALSE, color = FALSE) + #removing legends
  theme_bw() +
  theme(legend.position = "bottom")
 

# Cluster probabilities ---------------------------------------------------

nba_player_probs <- nba_mclust$z #probability of belonging in a component (rows sum to 1)

colnames(nba_player_probs) <- #grab column names and change it
  paste0('Cluster ', 1:3) #Cluster 1, Cluster 2, Cluster 3

#paste = concatenate words that are separated
#paste0 = not separated

nba_player_probs <- nba_player_probs %>% #1 row for each player cluster
  as_tibble() %>%
  mutate(player = 
           nba_filtered_stats$player) %>%
  pivot_longer(contains("Cluster"), #grab columns that contain Cluster OR pivot_longer(Cluster1:Cluster3)
               names_to = "cluster", 
               values_to = "prob")


#Graph of probabilities/uncertainty of clusters
## closer to 0.5 = more uncertainty

nba_player_probs %>%
  ggplot(aes(prob)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap(~ cluster, nrow = 2) #subplot for each cluster


#Which players have the highest uncertainty?

nba_filtered_stats %>%
  mutate(cluster =
           nba_mclust$classification,
         uncertainty =
           nba_mclust$uncertainty) %>%
  group_by(cluster) %>%
  arrange(desc(uncertainty)) %>% #highest uncertainty
  slice(1:5) %>% #top 5
  ggplot(aes(y = uncertainty, 
             x = reorder(player,
                         uncertainty))) +
  geom_point() +
  coord_flip() + 
  theme_bw() +
  facet_wrap(~ cluster, 
             scales = 'free_y', nrow = 3) #scales = y axis differs, nrow = 3 on top of eachother


#How uncertainty is calculated:
head(apply(nba_mclust$z, 1, function(x) 1-max(x))) #apply function by row
head(nba_mclust$uncertainty)








