# PURPOSE: K-means clustering of Gapminder data

# Load the data -----------------------------------------------------------

library(tidyverse)
library(dslabs)

gapminder <- as_tibble(gapminder)

gapminder %>% 
  ggplot(aes(x = log(gdp))) + 
  geom_histogram() #extremely skewed: problematic when dealing with distance


# Create clean data -------------------------------------------------------
## data for 2011 where gdp is not missing

clean_gapminder <- gapminder %>%
  filter(year == 2011, !is.na(gdp)) %>%
  mutate(log_gdp = log(gdp))
clean_gapminder

#Create scatterplot to see if there's clusters

clean_gapminder %>% 
  ggplot(aes(x = log_gdp, y = life_expectancy)) +
  geom_point(alpha = 0.75) +
  theme_bw()


# Initial unscaled k-means clustering --------------------------------------

init_kmeans <- 
  kmeans(dplyr::select(clean_gapminder,
                       log_gdp, life_expectancy),
         algorithm = "Lloyd", centers = 3,
         nstart = 1)


clean_gapminder %>%
  mutate(country_clusters = 
           as.factor(init_kmeans$cluster)) %>%
  ggplot(aes(x = log_gdp, y = life_expectancy,
             color = country_clusters)) +
  geom_point(alpha = 0.75) + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom") +
  coord_fixed()


# Standardize the variables -----------------------------------------------

clean_gapminder <- clean_gapminder %>% 
  mutate(std_log_gdp = (log_gdp - mean(log_gdp)) / sd(log_gdp),
         std_life_exp = (life_expectancy - mean(life_expectancy)) / sd(life_expectancy))

#OR

clean_gapminder <- clean_gapminder %>%
  mutate(std_log_gdp = as.numeric(scale(log_gdp, center = TRUE, scale = TRUE)),
         std_life_exp = as.numeric(scale(life_expectancy, center = TRUE, scale = TRUE)))

std_kmeans <- 
  kmeans(dplyr::select(clean_gapminder, std_log_gdp, std_life_exp),
         algorithm = "Lloyd", centers = 3, nstart = 30)

clean_gapminder %>%
  mutate(country_clusters = 
           as.factor(std_kmeans$cluster)) %>%
  ggplot(aes(x = log_gdp, y = life_expectancy,
             color = country_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() + 
  theme_bw() +
  theme(legend.position = "bottom")


# K-means ++ --------------------------------------------------------------

library(flexclust)

init_kmeanspp <- 
  kcca(dplyr::select(clean_gapminder,
                     std_log_gdp, std_life_exp), k = 3,
       control = list(initcent = "kmeanspp"))


clean_gapminder %>%
  mutate(country_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = log_gdp, y = life_expectancy,
             color = country_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")


# Heuristic: elbow plot ---------------------------------------------------

# Initialize number of clusters to search over
n_clusters_search <- 2:12
tibble(total_wss = 
         # Compute total WSS for each number by looping with sapply (into column)
         sapply(n_clusters_search,
                function(k) {
                  kmeans_results <- kmeans(dplyr::select(clean_gapminder,
                                                         std_log_gdp,
                                                         std_life_exp),
                                           centers = k, nstart = 30)
                  # Return the total WSS for choice of k
                  return(kmeans_results$tot.withinss)
                })) %>%
  mutate(k = n_clusters_search) %>% #another column with num of clusters
  ggplot(aes(x = k, y = total_wss)) +
  geom_line() + geom_point() +
  labs(x = "Number of clusters K", y = "Total WSS") +
  theme_bw()

##JUST A GUIDELINE, USE WITH CAUTION








