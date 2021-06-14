# Load Packages -----------------------------------------------------------
library(ggplot2)      # plotting
library(dplyr)        # data manipulation
library(tm)           # text mining
library(tidytext)     # text mining
library(themis)       # text mining
library(textfeatures) # extract additional class features
library(tidymodels)   # actual ML and feature engineering, class imbalance etc. 
library(textrecipes)  # for text feature in pipe
library(doParallel)   # for parallel computing
library(vip)          # explore variable importance
library(yaml)         # to save models

# Read in Data ------------------------------------------------------------
file_list <- list.files("./data/clean_data/individual_episodes", 
                        pattern = "*.csv", 
                        full.names = TRUE)

data <- do.call(rbind,lapply(file_list,read.csv))

# drop the epilogues and everything, which is not the actual episodes
data <- data %>%
  filter(episode != 12) %>% 
  filter(episode <= 115) %>%
  filter(segment != "break") %>%
  filter(segment != "extra") %>% 
  filter(!is.na(segment))

# only keep relevant variables for machine learning
data <- data %>% select(episode, arc, turn_number, segment, 
                        actor_guest, text, time_in_sec, 
                        words_per_minute, rp_combat)


# Cloud vs. Local Machine -------------------------------------------------

# cloud
doParallel::registerDoParallel(cores = detectCores())


# local
doParallel::registerDoParallel(cores = detectCores() -1 )
data <- data %>% slice(1:10000)


# Splitting into Training, Testing and Folds ------------------------------

# set = first episode airing
set.seed(20150312)

# splits
splits <- initial_split(data, strata = actor_guest)
train <- training(splits)
test <- testing(splits)

# cross validation folds
# seed for folds (airing first episode second campaign):
set.seed(20180111)
folds <- vfold_cv(train, strata = actor_guest)



# Recipe ------------------------------------------------------------------
cr_recipe <- recipe(actor_guest ~ text, data = train) %>% 
  step_downsample(actor_guest) %>%  # deal with class imbalance
  step_textfeature(text) %>%        # extract text features
  step_zv(all_predictors()) %>%     # remove everything with zero variance
  step_normalize(all_predictors())  # normalize all predictors



# Prep and Juice the Model (processioning and finalizing model) ------------
cr_prep <- prep(cr_recipe)    # compute recipe
cr_juiced <- juice(cr_prep)   # get preprocessed data



# Random Forrest ----------------------------------------------------------
# define it model
rf_spec <- rand_forest(
  mtry = tune(),        # number of predictors randomly sampled
  trees = 1000,         # numbers trees fitted
  min_n = tune()        # minimum number of data points in a node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# combine recipe and model to workflow
rf_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(rf_spec) 

# define search grid
rf_res <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = 40
)


# Find Best Model: Plotting -----------------------------------------------

rf_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


# Define new Grid by Hand and Rerun Model ---------------------------------

# new grid based on previous results
rf_grid <- grid_regular(
  min_n(range = c(15, 30)),
  mtry(range = c(2, 10)),
  levels = 1
)

# rerun model
rf_regular <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = rf_grid
)


# Select Best Model -------------------------------------------------------

rf_regular %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

rf_best_auc <- select_best(regular_rf, "roc_auc")



# Finalize Model ----------------------------------------------------------
rf_final <- finalize_model(
  tune_spec,
  rf_best_auc
)

rf_final



# Explore Model: Variable Importance --------------------------------------
rf_final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(actor_guest ~ .,
      data = juice(cr_prep) ) %>%
  vip(geom = "point")



# Finalize Model and Fit It -----------------------------------------------

# final work flow and fit
rf_final_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(rf_final)

# last prediction on test data
rf_last_fit <- rf_final_wf %>%
  last_fit(splits)

# collect AUC and accuracy
rf_last_fit %>%
  collect_metrics()


write_yaml(rf_final, "random_forrest.yml")















