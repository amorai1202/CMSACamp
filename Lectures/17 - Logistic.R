# PURPOSE: Demonstrate logistic regression models for FGs


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_fg_attempts <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/glm_examples/nfl_fg_attempt_data.csv")
nfl_fg_attempts


# Fit logit model ---------------------------------------------------------

## glm function (similar to lm) - specify the family is binomial

init_logit <- glm(is_fg_made ~ kick_distance,
                  data = nfl_fg_attempts,
                  family = "binomial")

## View predicted probability relationship

nfl_fg_attempts %>%
  mutate(pred_prob = init_logit$fitted.values) %>%
  ggplot(aes(x = kick_distance)) +
  geom_line(aes(y = pred_prob), #prob line
            color = "blue") +
  geom_point(aes(y = is_fg_made), #add 0 or 1 points 
             alpha = 0.3, 
             color = "darkorange") +
  theme_bw()

summary(init_logit)

logLik(init_logit)   # the maximum log-likelihood value



#Fitted values: estimate of the probability that the corresponding outcome Y is equal to 1

head(init_logit$fitted.values) 
summary(init_logit$fitted.values) #always lie in [0,1]

head(predict(init_logit)) #not [0,1] - on log-odds scale
head(predict(init_logit, type = "response")) #back on probability scale



# PREDICTIONS (classified p>0.5) ------------------------------------------

pred_fg_outcome <- ifelse(init_logit$fitted.values > 0.5,
                          "make", "miss")
#Matrix display
table("Predictions" = pred_fg_outcome, "Observed" = nfl_fg_attempts$is_fg_made)

#Misclassification rate
mean(ifelse(fitted(init_logit) < 0.5, 0, 1) != nfl_fg_attempts$is_fg_made)

#Brier score
mean((nfl_fg_attempts$is_fg_made - fitted(init_logit))^2)


# Calibration plots -------------------------------------------------------

nfl_fg_attempts %>%
  mutate(pred_prob = init_logit$fitted.values,
         bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
  # Group by bin_pred_prob:
  group_by(bin_pred_prob) %>%
  # Calculate the calibration results:
  summarize(n_attempts = n(), #num of rows
            bin_actual_prob = mean(is_fg_made)) %>% 
  #actual proportion that were made based on predicted prob
  
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_point(aes(size = n_attempts)) +
  geom_smooth(method = "loess", se = FALSE) + 
  geom_abline(slope = 1, intercept = 0, 
              color = "black", linetype = "dashed") + #want predictions to follow diagonal
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(size = "Number of attempts",
       x = "Estimated make probability",
       y = "Observed make probability") + 
  theme_bw() +
  theme(legend.position = "bottom")


# Leave-one-season-out cross validation (with purrr) ----------------------

nfl_fg_loso_cv_preds <- # generate holdout predictions for every row based season
  map_dfr(unique(nfl_fg_attempts$pbp_season), 
          function(season) {
            # Separate test and training data:
            test_data <- nfl_fg_attempts %>%
              filter(pbp_season == season)
            train_data <- nfl_fg_attempts %>%
              filter(pbp_season != season)
            # Train model:
            fg_model <- glm(is_fg_made ~ kick_distance, data = train_data,
                            family = "binomial")
            # Return tibble of holdout results:
            tibble(test_pred_probs = predict(fg_model, newdata = test_data,
                                             type = "response"),
                   test_actual = test_data$is_fg_made,
                   test_season = season) 
          })


#Misclassification rate

nfl_fg_loso_cv_preds %>%
  mutate(test_pred = ifelse(test_pred_probs < .5, 0, 1)) %>%
  summarize(mcr = mean(test_pred != test_actual))


#Brier score

nfl_fg_loso_cv_preds %>%
  summarize(brier_score = mean((test_actual - test_pred_probs)^2))

#Holdout performance by season

nfl_fg_loso_cv_preds %>%
  mutate(test_pred = ifelse(test_pred_probs < .5, 0, 1)) %>%
  group_by(test_season) %>%
  summarize(mcr = mean(test_pred != test_actual)) %>%
  ggplot(aes(x = test_season, y = mcr)) +
  geom_bar(stat = "identity", width = .1) + geom_point(size = 5) +
  theme_bw() +
  scale_x_continuous(breaks = unique(nfl_fg_loso_cv_preds$test_season))





