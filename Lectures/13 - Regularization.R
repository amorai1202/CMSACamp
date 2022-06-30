 # PURPOSE: Fit models with regularization


# Load data ---------------------------------------------------------------

library(tidyverse)
nfl_teams_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/regression_examples/nfl_team_season_summary.csv")
nfl_model_data <- nfl_teams_data %>%
  mutate(score_diff = points_scored - points_allowed) %>%
  # Only use rows with air yards
  filter(season >= 2006) %>%
  dplyr::select(-wins, -losses, -ties, -points_scored, -points_allowed, -season, -team)


# Begin using glmnet ------------------------------------------------------

library(glmnet)

# tidyverse way
model_x <- nfl_model_data %>%
  dplyr::select(-score_diff) %>% #get columns except for score_diff
  as.matrix()

#OR model.matrix way (dropping intercept)
model_x <- model.matrix(score_diff ~ ., nfl_model_data)[, -1] #drop the first column 
#period = use everything (all variables)


model_y <- nfl_model_data$score_diff #vector of score differential
#OR
model_y <- nfl_model_data %>% 
  pull(score_diff) #grab a column as a vector

init_lm <- lm(score_diff ~ ., nfl_model_data)
library(broom)
tidy(init_lm) %>% #table of summaries
  filter(term != "(Intercept)") %>% 
  mutate(coef_sign = as.factor(sign(estimate)), #make signs of estimate categorical in order to color
         term = fct_reorder(term, estimate)) %>% #reorder variable names by the estimate
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"), #red = negative, blue = positive
                    guide = FALSE) +
  coord_flip() + theme_bw()


# Ridge regression --------------------------------------------------------

init_ridge_fit <- glmnet(model_x, model_y, alpha = 0)
plot(init_ridge_fit, xvar = "lambda")
#Standardizes variables behind the scenes but return coefficients back on original scale

fit_ridge_cv <- cv.glmnet(model_x, model_y, alpha = 0) #default nfolds = 10
plot(fit_ridge_cv)


#Manual tidy ridge:

tidy_ridge_coef <- tidy(fit_ridge_cv$glmnet.fit) #path (step) of increasing lambda
tidy_ridge_coef %>%
  ggplot(aes(x = lambda, y = estimate, 
             group = term)) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = 
               fit_ridge_cv$lambda.min) +
  geom_vline(xintercept = 
               fit_ridge_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  theme_bw()

#Ridge regression coeff using 1 std err rule:

tidy_ridge_coef %>%
  filter(lambda == fit_ridge_cv$lambda.1se) %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"), guide = FALSE) +
  coord_flip() + theme_bw()


# Lasso regression --------------------------------------------------------

fit_lasso_cv <- cv.glmnet(model_x, model_y, 
                          alpha = 1)

#Tidy lasso:

tidy_lasso_coef <- tidy(fit_lasso_cv$glmnet.fit)

tidy_lasso_coef %>%
  ggplot(aes(x = lambda, y = estimate, 
             group = term)) + #group by variables to connect points
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = 
               fit_lasso_cv$lambda.min) + #first line
  geom_vline(xintercept = 
               fit_lasso_cv$lambda.1se, #1 std error rule line
             linetype = "dashed", color = "red") + 
  theme_bw()

#Show num of non-zero predictors:
tidy_lasso_cv <- tidy(fit_lasso_cv) #table of summary of cross-validation
tidy_lasso_cv %>%
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = fit_lasso_cv$lambda.min) +
  geom_vline(xintercept = fit_lasso_cv$lambda.1se, 
             linetype = "dashed", color = "red") +
  scale_x_log10() + theme_bw()


#Coeff using 1 std err rule:
tidy_lasso_coef %>%
  filter(lambda == fit_lasso_cv$lambda.1se) %>%
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, 
             fill = coef_sign)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_manual(values = c("darkred", "darkblue"), 
                    guide = FALSE) +
  coord_flip() +
  theme_bw()


# Elastic net examples ------------------------------------------------------------

set.seed(2020)
fold_id <- sample(rep(1:10, length.out = nrow(model_x))) #manually create our own folds

#Experiment with diff alphas
cv_en_25 <- cv.glmnet(model_x, model_y, 
                      foldid = fold_id, alpha = .25)
cv_en_50 <- cv.glmnet(model_x, model_y, 
                      foldid = fold_id, alpha = .5)
cv_ridge <- cv.glmnet(model_x, model_y, 
                      foldid = fold_id, alpha = 0)
cv_lasso <- cv.glmnet(model_x, model_y, 
                      foldid = fold_id, alpha = 1)

#Which has the smallest CV errors among candidate lambda values

which.min(c(min(cv_en_25$cvm), min(cv_en_50$cvm), 
            min(cv_ridge$cvm), min(cv_lasso$cvm)))  #cvm = min lambda

tidy(cv_en_50) %>%
  ggplot(aes(x = lambda, y = nzero)) +
  geom_line() +
  geom_vline(xintercept = cv_en_50$lambda.min) +
  geom_vline(xintercept = cv_en_50$lambda.1se, 
             linetype = "dashed", 
             color = "red") +
  scale_x_log10() + 
  theme_bw()

#Elastic net still does variable selection, but more relaxed than lasso
##Tune first, then see how they perform on holdout data


# Comparison of models based on holdout performance -----------------------

set.seed(2020)
nfl_model_data <- nfl_model_data %>% mutate(test_fold = sample(rep(1:5, length.out = n()))) # create 5 folds
holdout_predictions <- 
  map_dfr(unique(nfl_model_data$test_fold), 
          function(holdout) {
            # Separate test and training data:
            test_data <- nfl_model_data %>% filter(test_fold == holdout)
            train_data <- nfl_model_data %>% filter(test_fold != holdout)
            
            # Repeat for matrices
            test_x <- as.matrix(dplyr::select(test_data, -score_diff))
            train_x <- as.matrix(dplyr::select(train_data, -score_diff))
            
            # Train models:
            lm_model <- lm(score_diff ~ ., data = train_data)
            ridge_model <- cv.glmnet(train_x, train_data$score_diff, alpha = 0)
            lasso_model <- cv.glmnet(train_x, train_data$score_diff, alpha = 1)
            en_model <- cv.glmnet(train_x, train_data$score_diff, alpha = .5)
            
            # Return tibble of holdout results:
            tibble(lm_preds = predict(lm_model, newdata = test_data),
                   ridge_preds = as.numeric(predict(ridge_model, newx = test_x)),
                   lasso_preds = as.numeric(predict(lasso_model, newx = test_x)),
                   en_preds = as.numeric(predict(en_model, newx = test_x)),
                   test_actual = test_data$score_diff, test_fold = holdout) 
          })


#Compute RMSE - evaluating performance of diff models:

holdout_predictions %>%
  pivot_longer(lm_preds:en_preds, 
               names_to = "type", values_to = "test_preds") %>%
  group_by(type, test_fold) %>%
  summarize(rmse =
              sqrt(mean((test_actual - test_preds)^2))) %>% 
  ggplot(aes(x = type, y = rmse)) + 
  geom_point() + theme_bw() +
  stat_summary(fun = mean, geom = "point", 
               color = "red") + 
  stat_summary(fun.data = mean_se, geom = "errorbar",
               color = "red")









