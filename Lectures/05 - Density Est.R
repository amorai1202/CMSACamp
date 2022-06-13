# PURPOSE: Density estimation viz 

# Load data ---------------------------------------------------------------

library(tidyverse)
curry_shots <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/xy_examples/curry_2022_shots.csv")
head(curry_shots)


# Make histograms ---------------------------------------------------------

curry_shots %>% 
  ggplot(aes(x = shot_distance)) + 
  geom_histogram() +
  theme_bw()

# Noisy / spiky

curry_shots %>% 
  ggplot(aes(x = shot_distance)) + 
  geom_histogram(binwidth = 1) +
  theme_bw()

# Oversmooth / flat

curry_shots %>% 
  ggplot(aes(x = shot_distance)) + 
  geom_histogram(binwidth = 25) +
  theme_bw()

# Define more appropriate histogram breaks

curry_shots %>% 
  ggplot(aes(x = shot_distance)) +
  geom_histogram(binwidth = 1, center = 0.5, closed = "left") + #closed on the left hand side
  theme_bw()


# Density curves for 1D cont var -----------------------------------------------

curry_shots %>% 
  ggplot(aes(x = shot_distance)) + 
  geom_density() +
  geom_rug(alpha = 0.5) +
  theme_bw()

## Shrink h = sharp
curry_shots %>% 
  ggplot(aes(x = shot_distance)) + 
  geom_density(adjust = 0.5) +
  geom_rug(alpha = 0.5) +
  theme_bw()

## Expand h = overly smooth
curry_shots %>% 
  ggplot(aes(x = shot_distance)) + 
  geom_density(adjust = 3) +
  geom_rug(alpha = 0.5) +
  theme_bw()


# Density with ECDF

library(patchwork) #figures side-by-side

curry_density_curve <- curry_shots %>% 
  ggplot(aes(x = shot_distance)) + 
  geom_density() +
  geom_rug(alpha = 0.5) +
  theme_bw()

curry_ecdf <- curry_shots %>% 
  ggplot(aes(x = shot_distance)) +
  stat_ecdf() +
  theme_bw()

curry_density_curve + curry_ecdf

## Conditioned on if the shot is made

curry_shot_dens_made <- curry_shots %>%
  ggplot(aes(x = shot_distance, 
             color = is_shot_made)) + 
  geom_density() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Shot distance (in feet)",
       y = "Number of shot attempts")

curry_shot_ecdf_made <- curry_shots %>%
  ggplot(aes(x = shot_distance,
             color = is_shot_made)) + 
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Shot distance (in feet)",
       y = "Proportion of Curry shot attempts")
curry_shot_dens_made + curry_shot_ecdf_made + plot_layout(guides = 'collect')


## Look at by shot type - BAD

curry_shots %>% 
  ggplot(aes(x = shot_distance,
             color = shot_type)) + 
  geom_density() +
  geom_rug(alpha = 0.5) +
  theme_bw()

# Ridge Plots

library(ggridges)
curry_shots %>%
  ggplot(aes(x = shot_distance,
             y = shot_type)) +
  geom_density_ridges(rel_min_height = 0.01) +
  theme_bw()



# 2D shot location density estimation -------------------------------------

curry_shots %>%
  # Modify the shot coordinates
  mutate(shot_x = -shot_x / 10,
         shot_y = shot_y / 10) %>%
  ggplot(aes(x = shot_x, y = shot_y)) +
  #Adjust transparency
  geom_point(alpha = 0.3) +
  theme_bw()

# Contour plots

curry_shots <- curry_shots %>% 
  mutate(shot_x = -shot_x / 10,
         shot_y = shot_y / 10)

curry_shots %>% 
  ggplot(aes(x = shot_x, y = shot_y)) +
  geom_point(alpha = 0.3) +
  geom_density2d() + #adjust = num to change h
  theme_bw()

## Change units

curry_shots %>% 
  ggplot(aes(x = shot_x, y = shot_y)) + 
  geom_point(alpha = 0.3) + 
  geom_density2d() +
  theme_bw() + theme(legend.position = "bottom") +
  coord_fixed()

# Heat maps 

curry_shots %>% 
  ggplot(aes(x = shot_x, y = shot_y)) + 
  stat_density2d(h = 1, bins = 60,
                 aes(fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw() + theme(legend.position = "bottom") +
  coord_fixed()


## Use tiles
curry_shots %>% 
  ggplot(aes(x = shot_x, y = shot_y)) + 
  stat_density2d(h = 0.5, bins = 60,
                 contour = FALSE,
                 aes(fill = after_stat(density)),
                 geom = "raster") +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw() + theme(legend.position = "bottom") +
  coord_fixed()


# Hexagonal binning -------------------------------------------------------

#Shot frequencies

curry_shots %>% 
  ggplot(aes(x = shot_x, y = shot_y)) + 
  geom_hex(binwidth = c(1,1)) +
  scale_fill_gradient(low = "darkblue",
                      high = "darkorange") +
  theme_bw() + theme(legend.position = "bottom") +
  coord_fixed()


#Proportion of shots made

curry_shots %>% 
  ggplot(aes(x = shot_x, y = shot_y, 
             z = is_shot_made, #shooting efficiency
             group = -1)) +
  stat_summary_hex(binwidth = c(1, 1),
                   color = "black",
                   fun = mean) + #look at the mean/proportion of is_shot_made in each bin
  scale_fill_gradient(low = "darkblue", 
                      high = "darkorange") + 
  theme_bw() + theme(legend.position = "bottom") +
  coord_fixed()





