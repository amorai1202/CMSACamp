# PURPOSE: Explore KNN example/Kernel regression

## KNN might not be a good model to learn when the number of predictor variables (x) is very large

## KNN is not the best model to learn if inference is the goal of an analysis 
#KNN IS USED FOR PREDICTION!

## May need to scale (or standardize) the individual predictor variables 
##so that the distances are not skewed by that one predictor that has the largest variance

## FNN package in R has an option to search for nearest neighbors via the use of a kd tree



# Load the data -----------------------------------------------------------

library(tidyverse)
mlb_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/fg_batting_2021.csv")
head(mlb_data)


# Data cleaning -----------------------------------------------------------

#janitor package has convenient functions for data cleaning like clean_names()

#parse_number() function provides easy way to convert character to numeric columns

library(janitor)
mlb_data_clean <- clean_names(mlb_data)
mlb_data_clean <- mlb_data_clean %>%
  mutate_at(vars(bb_percent:k_percent), parse_number)
head(mlb_data_clean)


# KNN example using caret -------------------------------------------------

#caret is a package of functions designed to simplify training, tuning, and testing statistical learning methods

#first create partitions for training and test data using createDataPartition()

library(caret)
set.seed(1960)
train_i <- createDataPartition(y = mlb_data_clean$w_oba, p = 0.7, list = FALSE) %>%
  as.numeric()
train_mlb_data <- mlb_data_clean[train_i,]
test_mlb_data <- mlb_data_clean[-train_i,]

# next train() to find the optimal k on the training data with cross-validation

set.seed(1971)
init_knn_mlb_train <- train(w_oba ~ bb_percent + k_percent + iso, 
                            data = train_mlb_data, method = "knn",
                            trControl = trainControl("cv", number = 10),
                            preProcess = c("center", "scale"),
                            tuneLength = 10)

ggplot(init_knn_mlb_train) + theme_bw()

# can manually create a tuning grid to search over for the tuning parameter k

set.seed(1979)
tune_knn_mlb_train <- train(w_oba ~ bb_percent + k_percent + iso, 
                            data = train_mlb_data, method = "knn",
                            trControl = trainControl("cv", number = 10),
                            preProcess = c("center", "scale"),
                            tuneGrid = expand.grid(k = 2:20))
tune_knn_mlb_train$results

ggplot(tune_knn_mlb_train) + theme_bw()

tune_knn_mlb_train$bestTune #how is the optimal tuning parameter
test_preds <- predict(tune_knn_mlb_train, test_mlb_data) #predict on test
head(test_preds)
RMSE(test_preds, test_mlb_data$w_oba) #RMSE


# Kernel regression with np -----------------------------------------------
## APPLYING KERNELS IN THE REGRESSION SETTING TO GET WEIGHTED AVG OF LOCAL WINDOWS

# Use the npregbw function to tune bandwidth using generalized cross-validation

library(np)
mlb_bw0 <- npregbw(w_oba ~ bb_percent + k_percent + iso, 
                   data = train_mlb_data)

# Generate predictions with npreg with provided bandwidth object

mlb_test_npreg <- npreg(mlb_bw0, newdata = test_mlb_data)
RMSE(mlb_test_npreg$mean, test_mlb_data$w_oba)

