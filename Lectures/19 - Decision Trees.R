# PURPOSE: Explore modeling with decision trees


# Load and clean the data -------------------------------------------------

library(tidyverse)
mlb_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/fg_batting_2022.csv") %>%
  janitor::clean_names() %>% #clean the data
  mutate(across(bb_percent:k_percent, parse_number)) #convert percentages to numeric quantities
 # mutate_at(vars(bb_percent:k_percent), parse_number) 
 
head(mlb_data)


# Regression tree example -------------------------------------------------

library(rpart)
init_mlb_tree <- rpart(formula = w_oba ~ bb_percent + k_percent + iso,
                       data = mlb_data, method  = "anova")
init_mlb_tree #summary of the tree

library(rpart.plot)
#PLOT THE TREE
rpart.plot(init_mlb_tree) 

#10-fold CV error rate to find optimal alpha
plotcp(init_mlb_tree)

#Full tree - rpart.control
full_mlb_tree <- rpart(formula = w_oba ~ bb_percent + k_percent + iso,
                       data = mlb_data, method  = "anova",
                       control = list(cp = 0, xval = 10)) #cp = complexity
rpart.plot(full_mlb_tree) 
plotcp(full_mlb_tree)



# Caret example -----------------------------------------------------------

library(caret)
caret_mlb_tree <- train(w_oba ~ bb_percent + k_percent + iso + avg + obp + slg + war,
                        data = mlb_data, method = "rpart",
                        trControl = trainControl(method = "cv", number = 10),
                        tuneLength = 20)

ggplot(caret_mlb_tree) + theme_bw()

#PLOT TREE
rpart.plot(caret_mlb_tree$finalModel)


# Summarizing variables in tree-based models ------------------------------

# Variable importance based on reduction in SSE
library(vip)
vip(caret_mlb_tree, geom = "point") + theme_bw()

# Marginal relationships - Summarize single variable's relationship with partial dependence plot
library(pdp)
partial(caret_mlb_tree, pred.var = "obp") %>% autoplot() + theme_bw()



# Classification: predicting MLB HRs --------------------------------------

batted_ball_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/eda_projects/mlb_batted_balls_2022.csv") %>%
  mutate(is_hr = as.numeric(events == "home_run")) %>%
  filter(!is.na(launch_angle), !is.na(launch_speed),
         !is.na(is_hr))
table(batted_ball_data$is_hr)

#EDA scatterplot 

batted_ball_data %>%
  ggplot(aes(x = launch_speed, 
             y = launch_angle,
             color = as.factor(is_hr))) +
  geom_point(alpha = 0.5) +
  ggthemes::scale_color_colorblind(labels = c("No", "Yes")) +
  labs(x = "Exit velocity", 
       y = "Launch angle", 
       color = "HR?") +
  theme_bw() +
  theme(legend.position = "bottom")

#Train with caret

caret_hr_tree <- train(as.factor(is_hr) ~ launch_speed + launch_angle,
                       data = batted_ball_data, method = "rpart",
                       trControl = trainControl(method = "cv", number = 10),
                       tuneLength = 20)
ggplot(caret_hr_tree) + theme_bw()

#PLOT TREE

rpart.plot(caret_hr_tree$finalModel)


