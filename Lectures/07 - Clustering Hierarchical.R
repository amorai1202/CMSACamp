# PURPOSE: Hierarchical clustering of NBA players in 2021-2022 season


# Load data ---------------------------------------------------------------

library(tidyverse)
nba_pos_stats <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/clustering/nba_2022_player_per_pos_stats.csv")

# Find rows for players indicating a full season worth of stats
tot_players <- nba_pos_stats %>% filter(tm == "TOT")

# Stack this dataset with players that played on just one team
nba_player_stats <- nba_pos_stats %>% filter(!(player %in% tot_players$player)) %>% 
  bind_rows(tot_players) #removes all 3 rows for 1 play and add back 1 row of full season

# Look at ECDF to gauge minutes played

nba_player_stats %>% 
  ggplot(aes(x = mp)) +
  stat_ecdf() +
  geom_vline(xintercept = 125) #cutoff at 125 mp
  theme_bw()
  
  #geom_hline

# Filter to only players with at least 125 minutes played
nba_filtered_stats <- nba_player_stats %>% filter(mp >= 125)
head(nba_filtered_stats)


# Plot trb and x3pa -------------------------------------------------------

nba_filtered_stats %>% 
  ggplot(aes(x = x3pa, y = trb)) +
  geom_point(alpha = 0.5) +
  theme_bw()


# Compute player distance matrix ------------------------------------------

player_dist <- dist(dplyr::select(nba_filtered_stats,
                                  x3pa, trb))

# Create matrix to explore 
player_dist_matrix <- as.matrix(player_dist)

rownames(player_dist_matrix) <- nba_filtered_stats$player
colnames(player_dist_matrix) <- nba_filtered_stats$player
head(player_dist_matrix[1:3, 1:3])

# Pivot matrix in order to plot

long_dist_matrix <- 
  as_tibble(player_dist_matrix) %>%
  mutate(player1 = rownames(player_dist_matrix)) %>%
  pivot_longer(cols = -player1,  #ignore player1 when pivoting
               names_to = "player2",
               values_to = "distance")

#Tile plot to visualize distance

long_dist_matrix %>%
  ggplot(aes(x = player1, y = player2, 
             fill = distance)) +
  geom_tile() +
  theme_bw() +
  theme(axis.text = element_blank(), #remove axis labels, tick marks 
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  scale_fill_gradient(low = "darkorange", 
                      high = "darkblue")

# Rearrange order to get distances close together

library(seriation)

player_dist_seriate <- seriate(player_dist)
player_order <- get_order(player_dist_seriate) #optimal arrangement

player_names_order <- 
  nba_filtered_stats$player[player_order] #apply arrangement


long_dist_matrix %>%
  mutate(player1 = 
           fct_relevel(player1, 
                       player_names_order),
         player2 = 
           fct_relevel(player2, 
                       player_names_order)) %>%
  ggplot(aes(x = player1, y = player2, 
             fill = distance)) +
  geom_tile() + theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  scale_fill_gradient(low = "darkorange",
                      high = "darkblue")


# Complete linkage with hclust --------------------------------------------

player_dist <- dist(dplyr::select(nba_filtered_stats,
                                  x3pa, trb))
nba_complete_hclust <- 
  hclust(player_dist, method = "complete")


nba_filtered_stats %>%
  mutate(player_clusters = 
           as.factor(cutree(nba_complete_hclust, k = 3))) %>% 
  ggplot(aes(x = x3pa, y = trb,
             color = player_clusters)) +
  geom_point(alpha = 0.5) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

# Look at dendrogram

library(ggdendro)
ggdendrogram(nba_complete_hclust, theme_dendro = FALSE,
             labels = FALSE, leaf_labels = FALSE) +
  labs(y = "Dissimilarity between clusters") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())






