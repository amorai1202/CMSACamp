# PURPOSE: Demo random forests


# Load the data -----------------------------------------------------------

library(tidyverse)
mlb_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/fg_batting_2022.csv") %>%
  janitor::clean_names() %>%
  mutate_at(vars(bb_percent:k_percent), parse_number)

model_mlb_data <- mlb_data %>%
  dplyr::select(-name, -team, -playerid)
head(model_mlb_data)


# Fit random forest -------------------------------------------------------

library(ranger)
init_mlb_rf <- ranger(war ~ ., data = model_mlb_data, 
                      num.trees = 50, importance = "impurity") # impurity contains SSE
init_mlb_rf

# Look at variable importance - display top 10 (default) most important variables

library(vip) 
vip(init_mlb_rf, geom = "point") + theme_bw() #points
vip(init_mlb_rf, geom = "col") + theme_bw() #columns

# Tuning random forests

library(caret)
rf_tune_grid <- 
  expand.grid(mtry = seq(3, 18, by = 3), 
              splitrule = "variance",
              min.node.size = 5)
set.seed(1917)
caret_mlb_rf <- 
  train(war ~ ., data = model_mlb_data,
        method = "ranger", num.trees = 50,
        trControl = trainControl(method = "cv", number = 5),
        tuneGrid = rf_tune_grid)

ggplot(caret_mlb_rf) + theme_bw()



