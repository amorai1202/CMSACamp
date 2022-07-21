# PURPOSE: Multinomial logit regression and mixed effects models


# Load the data -----------------------------------------------------------

library(tidyverse)
nfl_ep_model_data <- read_rds(url("http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/model_pbp_data.rds"))
nfl_ep_model_data <- nfl_ep_model_data %>%
  mutate(Next_Score_Half = fct_relevel(Next_Score_Half, "No_Score"), #move no_score to front to make it the reference
         # log transform of yards to go and indicator for two minute warning:
         log_ydstogo = log(ydstogo),
         # Changing down into a factor variable: 
         down = factor(down))


# Fit multinomial logit model ---------------------------------------------

library(nnet)

init_ep_model <- multinom(Next_Score_Half ~ half_seconds_remaining + yardline_100 + down + log_ydstogo + log_ydstogo*down + yardline_100*down, 
                          data = nfl_ep_model_data, maxit = 300) #maxit = steps/iterations in model

summary(init_ep_model)

# Leave-one-season-out cross-validation

init_loso_cv_preds <- 
  map_dfr(unique(nfl_ep_model_data$season), #loop over every season
          function(x) { #x = current season
            # Separate test and training data:
            test_data <- nfl_ep_model_data %>% filter(season == x)
            train_data <- nfl_ep_model_data %>% filter(season != x)
            
            # Fit multinomial logistic regression model:
            ep_model <- 
              multinom(Next_Score_Half ~ half_seconds_remaining + yardline_100 + down + log_ydstogo + 
                         log_ydstogo*down + yardline_100*down, data = train_data, maxit = 300)
            
            # Return dataset of class probabilities:
            predict(ep_model, newdata = test_data, type = "probs") %>% #matrix with col for events
              as_tibble() %>%
              mutate(Next_Score_Half = test_data$Next_Score_Half,
                     season = x) #add column for season
          })



# Load CV results ---------------------------------------------------------

init_loso_cv_preds <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/init_nfl_ep_loso_cv_preds.csv")


# Calibration results for each scoring event ----------------------------------

ep_cv_loso_calibration_results <- init_loso_cv_preds %>%
  pivot_longer(No_Score:Touchdown, #each outcome will have 7 rows 
               names_to = "next_score_type",
               values_to = "pred_prob") %>%
  
  mutate(bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
  group_by(next_score_type, bin_pred_prob) %>% #make plot for each individual scoring outcome
  summarize(n_plays = n(), 
            n_scoring_event = length(which(Next_Score_Half == next_score_type)), #count how many trues
            bin_actual_prob = n_scoring_event / n_plays,
            bin_se = sqrt((bin_actual_prob * (1 - bin_actual_prob)) / n_plays)) %>% #uncertainty of estimate (p*q/n)
  ungroup() %>%
  mutate(bin_upper = pmin(bin_actual_prob + 2 * bin_se, 1), #confidence intervals capped at 0 and 1
         bin_lower = pmax(bin_actual_prob - 2 * bin_se, 0))


#Calibration plots - ASSESSING MODELING RESULTS

ep_cv_loso_calibration_results %>%
  mutate(next_score_type = fct_relevel(next_score_type, "Opp_Safety", "Opp_Field_Goal", 
                                       "Opp_Touchdown", "No_Score", "Safety", "Field_Goal", "Touchdown"),
         next_score_type = fct_recode(next_score_type, "-Field Goal (-3)" = "Opp_Field_Goal", "-Safety (-2)" = "Opp_Safety", "-Touchdown (-7)" = "Opp_Touchdown",
                                      "Field Goal (3)" = "Field_Goal", "No Score (0)" = "No_Score",
                                      "Touchdown (7)" = "Touchdown", "Safety (2)" = "Safety")) %>% #cosmetic
  
  ggplot(aes(x = bin_pred_prob, y = bin_actual_prob)) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") + #45 degree line
  geom_smooth(se = FALSE) + #smoother reference line
  geom_point(aes(size = n_plays)) +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) + #coord_equal() +   #error bar
  scale_x_continuous(limits = c(0,1)) +  
  scale_y_continuous(limits = c(0,1)) + 
  labs(size = "Number of plays", x = "Estimated next score probability", 
       y = "Observed next score probability") + 
  theme_bw() + 
  theme(strip.background = element_blank(), 
        axis.text.x = element_text(angle = 90), 
        legend.position = c(1, .05), legend.justification = c(1, 0)) +
  facet_wrap(~ next_score_type, ncol = 4) #plot for each score type



# Evaluating players - Expected points added (EPA) -------------------------------------

## Goal: divide credit between players involved in a play, i.e. who deserves what portion of EPA? 
## Data displays group structure and different levels of variation within groups
  
#Load dataset of 2021 passing plays:
  
nfl_passing_plays <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/sports/eda_projects/nfl_passing_plays_2021.csv") %>%
  # Only keep rows with passer and receiver information known:
  filter(!is.na(passer_player_id), !is.na(receiver_player_id), !is.na(epa)) %>%
  # Combine passer and receiver unique IDs:
  mutate(passer_name_id = paste0(passer_player_name, ":", passer_player_id),
         receiver_name_id = paste0(receiver_player_name, ":", receiver_player_id))



# Fit mixed/multilevel models -------------------------------------------------------

library(lme4)
passing_lmer <- lmer(epa ~ shotgun + air_yards + (1|passer_name_id) + (1|receiver_name_id),
                     data = nfl_passing_plays) #adding random effects on categorical variables
summary(passing_lmer)

# Variance partition coefficients and intraclass correlations

VarCorr(passing_lmer) %>% as_tibble() %>% 
  mutate(icc = vcov / sum(vcov)) %>% dplyr::select(grp, icc)


# Simulations for model performance using merTools ------------------------

##Compare random effects with uncertainty via parametric bootstrapping

library(merTools)
player_effects <- REsim(passing_lmer) #200 simulations to get uncertainty intervals
plotREsim(player_effects) 

# Get best and worst players

player_effects %>%
  as_tibble() %>%
  group_by(groupFctr) %>%
  arrange(desc(mean)) %>%
  slice(1:5, (n() - 4):n()) %>% #top 5 and bottom 5 for each QB and receiver
  
  ggplot(aes(x = reorder(groupID, mean))) + #high to low
  geom_point(aes(y = mean)) +
  geom_errorbar(aes(ymin = mean - 2 * sd, #manually add error bars
                    ymax = mean + 2 * sd)) +
  coord_flip() + #names on y-axis
  facet_wrap(~groupFctr, ncol = 1, scales = "free_y") + # plot separately by position
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "red") +
  theme_bw()


