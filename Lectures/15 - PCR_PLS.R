# PURPOSE: Use PCR and PLS models


# Load the data -----------------------------------------------------------

library(tidyverse)

#Read in as csv
nfl_model_data <- read_csv("Data/nfl_model_data.csv")

#Read in as rds
nfl_model_data <- read_rds("Data/nfl_model_data.rds")



# Fit PCR -----------------------------------------------------------------

library(pls)
nfl_pcr_fit <- pcr(score_diff ~ ., ncomp = 2, 
                   scale = TRUE, data = nfl_model_data)

summary(nfl_pcr_fit)


# Tune PCR with caret -----------------------------------------------------

set.seed(2013)

library(caret)

# Tune num of components in PCR with caret & train with 10-fold CV using pcr from pls

cv_model_pcr <- train(
  score_diff ~ ., 
  data = nfl_model_data, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10, #10 folds
                           selectionFunction = "oneSE"), #implementing 1 SE rule
  preProcess = c("center", "scale"),
  tuneLength = ncol(nfl_model_data) - 1) #remove response variable


ggplot(cv_model_pcr) +
  theme_bw()

# Returns model with minimum CV error as finalModel

summary(cv_model_pcr$finalModel)


# Tune PLS with caret -----------------------------------------------------

cv_model_pls <- train(
  score_diff ~ ., 
  data = nfl_model_data, 
  method = "pls", #CHANGE METHOD
  trControl = trainControl(method = "cv", number = 10, #10 folds
                           selectionFunction = "oneSE"), #implementing 1 SE rule
  preProcess = c("center", "scale"),
  tuneLength = ncol(nfl_model_data) - 1) 

ggplot(cv_model_pls) +
  theme_bw()


# Variable importance with vip package ------------------------------------

## Check out `cv_model_pls$finalModel$coefficients`

library(vip)
vip(cv_model_pls, num_features = 10, #10 most influential variables in the model
    method = "model") +
  theme_bw()


# Partial dependence plots (PDP) ------------------------------------------

## Change in the average predicted response as the predictor varies over their marginal distribution
## More useful for non-linear models

library(pdp)
partial(cv_model_pls, "offense_total_epa_pass", plot = TRUE)










