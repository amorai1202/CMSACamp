# PURPOSE: Explore variable correlations


# Load data ---------------------------------------------------------------

library(tidyverse)
nfl_teams_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/regression_examples/nfl_team_season_summary.csv")
nfl_teams_data


nfl_teams_data <- nfl_teams_data %>%
  mutate(score_diff = points_scored - points_allowed)


hist(nfl_teams_data$score_diff)

#Correlation plot
library(ggcorrplot)
nfl_model_data <- nfl_teams_data %>%
  dplyr::select(score_diff, offense_ave_epa_pass,
                offense_ave_epa_run, 
                defense_ave_epa_pass,
                defense_ave_epa_run,
                offense_ave_yards_gained_pass,
                offense_ave_yards_gained_run,
                defense_ave_yards_gained_pass,
                defense_ave_yards_gained_run)
nfl_cor_matrix <- round(cor(nfl_model_data), digits = 2)
ggcorrplot(nfl_cor_matrix, type = "lower", lab = TRUE,
           hc.order = TRUE)


nfl_ex_vars <- dplyr::select(nfl_model_data, -score_diff)
ex_cor_matrix <- cor(nfl_ex_vars)
cor_dist_matrix <- 1 - abs(ex_cor_matrix)
cor_dist_matrix <- as.dist(cor_dist_matrix)

#Clustering variables using correlation matrix
nfl_ex_hc <- hclust(cor_dist_matrix, "complete")
library(ggdendro)
ggdendrogram(nfl_ex_hc, rotate = TRUE, size = 2) #close together = correlated


#Pairs plot of multiple variables
library(GGally)
ggpairs(nfl_model_data,
        columns = c("score_diff", "offense_ave_epa_run",
                    "offense_ave_epa_pass", 
                    "defense_ave_epa_run", 
                    "defense_ave_epa_pass"),
        aes(alpha = .5)) #need to add


# Perform 5-fold CV -------------------------------------------------------

set.seed(2020)
nfl_model_data <- nfl_model_data %>% 
  mutate(test_fold = sample(rep(1:5, length.out = n()))) #n() - special function to count rows

head(nfl_model_data$test_fold)
table(nfl_model_data$test_fold) #observations assigned to 5 different test folds

#Get cross validation predictions
#INPUTS: model_formula, dataset (where dataset assumed to have test_folds)
#OUTPUT: Dataset containing test fold predictions

get_cv_preds <- function(model_formula, data = nfl_model_data) {
  # generate holdout predictions for every row based season
  map_dfr(unique(data$test_fold), 
          function(holdout) {
            
            # Separate test and training data:
            test_data <- data %>%
              filter(test_fold == holdout)
            
            train_data <- data %>%
              filter(test_fold != holdout)
            
            # Fit the model on training model:
            train_model <- lm(as.formula(model_formula), data = train_data)
            
            # Return tibble (a table) of holdout results:
            tibble(test_preds = predict(train_model, newdata = test_data),
                   test_actual = test_data$score_diff,
                   test_fold = holdout) 
          })
}

#Get CV results with different sets of variables: Apply function over variables

all_cv_preds <- get_cv_preds("score_diff ~  offense_ave_epa_pass + offense_ave_epa_run + defense_ave_epa_pass + defense_ave_epa_run")
all_int_cv_preds <- get_cv_preds("score_diff ~ offense_ave_epa_pass*offense_ave_epa_run + defense_ave_epa_pass*defense_ave_epa_run")
run_only_cv_preds <- get_cv_preds("score_diff ~ offense_ave_epa_run + defense_ave_epa_run")
pass_only_cv_preds <- get_cv_preds("score_diff ~ offense_ave_epa_pass + defense_ave_epa_pass")
off_only_cv_preds <- get_cv_preds("score_diff ~ offense_ave_epa_pass + offense_ave_epa_run")
def_only_cv_preds <- get_cv_preds("score_diff ~ defense_ave_epa_pass + defense_ave_epa_run")
int_only_cv_preds <- get_cv_preds("score_diff ~ 1") #NECESSARY BASELINE 

#Summarize for a single plot:

bind_rows(mutate(all_cv_preds, type = "All"),
          mutate(all_int_cv_preds, type = "All w/ interactions"),
          mutate(pass_only_cv_preds, type = "Passing only"),
          mutate(run_only_cv_preds, type = "Running only"),
          mutate(off_only_cv_preds, type = "Offense only"),
          mutate(def_only_cv_preds, type = "Defense only"),
          mutate(int_only_cv_preds, type = "Intercept-only")) %>%
  group_by(type) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2))) %>%
  mutate(type = fct_reorder(type, rmse)) %>%
  ggplot(aes(x = type, y = rmse)) +
  geom_bar(stat = "identity") +   #geom_point()
  coord_flip()+ 
  theme_bw()

#Holdout performance for each particular fold

bind_rows(mutate(all_cv_preds, type = "All"),
          mutate(all_int_cv_preds, type = "All w/ interactions"),
          mutate(pass_only_cv_preds, type = "Passing only"),
          mutate(run_only_cv_preds, type = "Running only"),
          mutate(off_only_cv_preds, type = "Offense only"),
          mutate(def_only_cv_preds, type = "Defense only"),
          mutate(int_only_cv_preds, type = "Intercept-only")) %>%
  group_by(type, test_fold) %>%
  summarize(rmse = sqrt(mean((test_actual - test_preds)^2))) %>%
  mutate(type = fct_reorder(type, rmse)) %>%
  ggplot(aes(x = type, y = rmse)) +
  geom_point(alpha = .8) +
  coord_flip()+ 
  theme_bw()


# Fit selected model on all data ------------------------------------------

all_lm <- lm(score_diff ~ offense_ave_epa_pass + offense_ave_epa_run + defense_ave_epa_pass + defense_ave_epa_run, data = nfl_model_data)
summary(all_lm)

#Display confidence intervals (better for a presentation)

ggcoef(all_lm, 
       exclude_intercept = TRUE,
       vline = TRUE,
       vline_color = "red") + 
  theme_bw()





