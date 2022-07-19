# PURPOSE: Demo gradient boosted trees

# Load the data -----------------------------------------------------------

library(tidyverse)
mlb_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/fg_batting_2022.csv") %>%
  janitor::clean_names() %>%
  mutate_at(vars(bb_percent:k_percent), parse_number)

model_mlb_data <- mlb_data %>%
  dplyr::select(-name, -team, -playerid)
head(model_mlb_data)


# Try GBM -----------------------------------------------------------------

library(xgboost)
xgboost_tune_grid <- expand.grid(nrounds = seq(from = 20, to = 200, by = 20),
                                 eta = c(0.025, 0.05, 0.1, 0.3), gamma = 0,
                                 max_depth = c(1, 2, 3, 4), colsample_bytree = 1,
                                 min_child_weight = 1, subsample = 1)
xgboost_tune_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
set.seed(1937)
xgb_tune <- train(x = as.matrix(dplyr::select(model_mlb_data, -war)),
                  y = model_mlb_data$war, trControl = xgboost_tune_control,
                  tuneGrid = xgboost_tune_grid, 
                  objective = "reg:squarederror", method = "xgbTree",
                  verbose = TRUE)

# Extreme grading boosting - XGBoost

xgb_fit_final <- xgboost(data = as.matrix(dplyr::select(model_mlb_data, -war)),
                         label = model_mlb_data$war, objective = "reg:squarederror",
                         nrounds = xgb_tune$bestTune$nrounds,
                         params = as.list(dplyr::select(xgb_tune$bestTune,
                                                        -nrounds)), 
                         verbose = 0)

# Variable importance
vip(xgb_fit_final) + theme_bw()

# Partial dependence plots
library(pdp)
partial(xgb_fit_final, pred.var = "off", train = as.matrix(dplyr::select(model_mlb_data, -war)),
        plot.engine = "ggplot2", plot = TRUE,
        type = "regression") + theme_bw()


