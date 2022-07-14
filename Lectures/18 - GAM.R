# PURPOSE: GAM - Predicting MLB HR probability


# Load the data -----------------------------------------------------------

library(tidyverse)
batted_ball_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/eda_projects/mlb_batted_balls_2022.csv") %>%
  mutate(is_hr = as.numeric(events == "home_run")) %>%
  filter(!is.na(launch_angle), !is.na(launch_speed),
         !is.na(is_hr))
head(batted_ball_data)


# Visualize HRs -----------------------------------------------------------

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


# Use mgcv to model HR prob -----------------------------------------------

set.seed(2004)
batted_ball_data <- batted_ball_data %>%
  mutate(is_train = sample(rep(0:1, 
                               length.out = nrow(batted_ball_data)))) 
#set up training data (randomly assigning training/testing data)

library(mgcv)
init_logit_gam <- gam(is_hr ~ s(launch_speed) + s(launch_angle), #smooth function
                      data = filter(batted_ball_data, is_train == 1), 
                      family = binomial, method = "REML")

summary(init_logit_gam)

# Display the partial effect of each term in the model -> add up to the overall prediction

library(gratia)
draw(init_logit_gam)

# Convert to probability scale with plogis function

draw(init_logit_gam, fun = plogis) #without the intercept 

draw(init_logit_gam, fun = plogis, 
     constant = coef(init_logit_gam)[1]) #intercept reflects relatively rare occurence of HRs!

# Check to see if we need more basis functions based on an approximate test
gam.check(init_logit_gam) #lower p-value may indicate k needs to be higher


# Check the predictions ---------------------------------------------------

batted_ball_data <- batted_ball_data %>%
  mutate(init_gam_hr_prob = 
           as.numeric(predict(init_logit_gam,
                              newdata = batted_ball_data,
                              type = "response")), #original scale
         init_gam_hr_class = as.numeric(init_gam_hr_prob >= 0.5))
        # init_gam_hr_class = ifelse(init_gam_hr_prob >= 0.5, 1))

batted_ball_data %>%
  group_by(is_train) %>%
  summarize(correct = mean(is_hr == init_gam_hr_class))

# Compare with linear model

init_linear_logit <- glm(is_hr ~ launch_speed + launch_angle, 
                         data = filter(batted_ball_data, is_train == 1), 
                         family = binomial)
batted_ball_data <- batted_ball_data %>%
  mutate(init_glm_hr_prob = predict(init_linear_logit,
                                    newdata = batted_ball_data,
                                    type = "response"),
         init_glm_hr_class = as.numeric(init_glm_hr_prob >= 0.5))

batted_ball_data %>%
  group_by(is_train) %>%
  summarize(correct = mean(is_hr == init_glm_hr_class))



# Continuous interactions -------------------------------------------------

multi_logit_gam <- gam(is_hr ~ s(launch_speed, launch_angle), 
                       data = filter(batted_ball_data, is_train == 1), 
                       family = binomial)

draw(multi_logit_gam)

# Check predictions

batted_ball_data <- batted_ball_data %>%
  mutate(multi_gam_hr_prob =
           as.numeric(predict(multi_logit_gam,
                              newdata = batted_ball_data,
                              type = "response")),
         multi_gam_hr_class = as.numeric(multi_gam_hr_prob >= 0.5))
batted_ball_data %>%
  group_by(is_train) %>%
  summarize(correct = mean(is_hr == multi_gam_hr_class))

# Separate interactions from individual terms with tensor smooths

tensor_logit_gam <- gam(is_hr ~ s(launch_speed) + s(launch_angle) +
                          ti(launch_speed, launch_angle),
                        data = filter(batted_ball_data, is_train == 1), family = binomial)
# Check the predictions

batted_ball_data %>%
  mutate(tensor_gam_hr_prob =
           as.numeric(predict(tensor_logit_gam, newdata = batted_ball_data, type = "response")),
         tensor_gam_hr_class = as.numeric(tensor_gam_hr_prob >= 0.5)) %>%
  group_by(is_train) %>% 
  summarize(correct = mean(is_hr == tensor_gam_hr_class))

## More complicated model but yet it does not help!



