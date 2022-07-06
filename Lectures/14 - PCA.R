# PURPOSE: PCA on NFL teams data


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_teams_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/regression_examples/nfl_team_season_summary.csv")
nfl_model_data <- nfl_teams_data %>%
  mutate(score_diff = points_scored - points_allowed) %>%
  # Only use rows with air yards
  filter(season >= 2006) %>%
  dplyr::select(-wins, -losses, -ties, -points_scored, -points_allowed, -season, -team)



# Run PCA -----------------------------------------------------------------

#Create matrix
model_x <- as.matrix(dplyr::select(nfl_model_data, -score_diff)) #removing score_diff b/c we're interested in it

#Use the prcomp function (uses SVD) for PCA on centered and scaled data
pca_nfl <- prcomp(model_x, center = TRUE, scale = TRUE) 

summary(pca_nfl) #summarizes how much variances are being explaining by various components

pca_nfl$sdev #corresponds to the singular values of matrix D

#Proportion of variance explained
pca_nfl$sdev^2 / ncol(model_x) 

#Plotting
library(broom)
pca_nfl %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 1 / ncol(model_x), # 1/num of possible PC's (dimensions in data = p)
             color = "darkred", 
             linetype = "dashed") +
  theme_bw()

#Matrix of principal component scores (Z = XV)
pca_nfl$x #sign does not matter


# Visualize the proportion of variance explained by each PC  --------

library(factoextra) #useful for plotting PCA summaries

#Scree plot
fviz_eig(pca_nfl) +
  geom_hline(yintercept = 1 / ncol(model_x), 
             color = "darkred", 
             linetype = "dashed") 

#Display observations with first two PC
fviz_pca_ind(pca_nfl)

#Projection of variables - 
##angles are interpreted as correlations, where negative correlated values point to opposite sides of graph

fviz_pca_var(pca_nfl)


#Biplot displays both the space of observations and the space of variables 
##arrows represent the directions of the original variables - 90 degrees = independent

fviz_pca_biplot(pca_nfl, label = "var", 
                alpha.ind = .5, col.var = "darkblue",
                alpha.var = .75)







