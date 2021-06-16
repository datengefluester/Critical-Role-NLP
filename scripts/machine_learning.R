# Load Packages -----------------------------------------------------------
library(ggplot2)      # plotting
library(dplyr)        # data manipulation
library(tidytext)     # text mining
library(themis)       # text mining
library(textfeatures) # extract additional class features
library(tidymodels)   # actual ML and feature engineering, class imbalance etc. 
library(textrecipes)  # for text feature in pipe
library(doParallel)   # for parallel computing
library(vip)          # explore variable importance
library(discrim)      # for naive Bayes


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
  filter(!is.na(segment)) %>% 
  filter(!is.na(rp_combat))

# also drop every speaker, which contains 'AND' as these are multiple speakers,
# which increases the amount of classes to be predicted significantly and 
# decrease the sample size by way too much to allow for a balanced sample
data <- data %>% 
  filter(actor_guest != "ZAC") %>%
  filter(actor_guest != "BRIAN") %>%
  filter(!grepl("AND", actor_guest))

# only keep relevant variables for machine learning
data <- data %>% select(actor_guest, episode, arc, turn_number, segment, 
                        text, time_in_sec, words_per_minute, rp_combat)

# transform rp_combat to be more intuitive
data <- data %>% 
  rename(combat = rp_combat) %>% 
  mutate(combat = replace(combat, combat=="role_play", 0)) %>% 
  mutate(combat = replace(combat, combat=="combat", 1)) 

# Cloud vs. Local Machine -------------------------------------------------

# cloud
doParallel::registerDoParallel(cores = detectCores())


# local
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
cr_recipe <- recipe(actor_guest ~ text +time_in_sec + words_per_minute + arc + segment + combat , data = train) %>% 
  step_downsample(actor_guest) %>%        # deal with class imbalance
  step_dummy(arc, segment) %>%            # convert factor variables into multiple dummy variables
  step_textfeature(text) %>%              # extract text features
  step_zv(all_predictors()) %>%           # remove everything with zero variance
  step_normalize(time_in_sec, words_per_minute, contains("textfeature"))  %>%       # normalize numeric predictors
  step_lincomb(all_numeric())             # remove any linear combinations



# Prep and Juice the Model (processioning and finalizing model) ------------
cr_prep <- prep(cr_recipe)    # compute recipe
cr_juiced <- juice(cr_prep)   # get pre-processed data


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
  control = control_grid(parallel_over = "everything"), # parallel tuning
)


# Find Best Model: Plotting 
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


# Define new Grid by Hand and Rerun Model

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


# Select Best Model 
rf_regular %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

rf_best_auc <- select_best(rf_regular, "roc_auc")



# Finalize Model 
rf_final <- finalize_model(
  rf_spec,
  rf_best_auc
)

rf_final



# Explore Model: Variable Importance 
rf_final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(actor_guest ~ .,
      data = juice(cr_prep) ) %>%
  vip(geom = "point")

# final work flow and fit
rf_final_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(rf_final)

# last prediction on test data
rf_last_fit <- 
  rf_final_wf %>%
  last_fit(splits)

# collect AUC and accuracy
rf_last_fit %>%
  collect_metrics()

# create data frame for overview for all models
fit <- rf_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "random_forest") %>% 
  select(model, roc_auc, accuracy)


# extract final model and save it
rf_final_model <- rf_last_fit$.workflow[[1]]

# check if you get prediction out
predict(rf_final_model, data[222,])

# save model
saveRDS(rf_final_model, "./output/models/rf.rds")



# SVM ---------------------------------------------------------------------
svm_spec <-
  svm_rbf(cost = tune(), 
          rbf_sigma = tune(), 
          margin = tune()) %>%
  set_engine('kernlab') %>%
  set_mode('classification')

# combine recipe and model to workflow
svm_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(svm_spec) 

# define search grid
svm_res <- tune_grid(
  svm_wf,
  resamples = folds,
  control = control_grid(parallel_over = "everything")
)

# Select Best Model 
svm_best_auc <- select_best(svm_res, "roc_auc")


# Finalize Model 
svm_final <- finalize_model(
  svm_spec,
  svm_best_auc
)


# final work flow and fit
svm_final_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(svm_final)

# last prediction on test data
svm_last_fit <- 
  svm_final_wf %>%
  last_fit(splits)

# collect AUC and accuracy
svm_last_fit %>%
  collect_metrics()

# add to data frame for overview
fit <- svm_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "svm") %>% 
  bind_rows(fit)
  
# extract final model and save it
svm_final_model <- svm_last_fit$.workflow[[1]]

# check if you get prediction out
predict(svm_final_model, data[222,])

# save model
saveRDS(svm_final_model, "./output/models/svm.rds")




# Decision Tree -----------------------------------------------------------

tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")


# combine recipe and model to workflow
tree_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(tree_spec) 

# define search grid
tree_res <- tune_grid(
  tree_wf,
  resamples = folds,
  control = control_grid(parallel_over = "everything")
)

# Select Best Model 
tree_best_auc <- select_best(tree_res, "roc_auc")


# Finalize Model 
tree_final <- finalize_model(
  tree_spec,
  tree_best_auc
)


# final work flow and fit
tree_final_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(tree_final)

# last prediction on test data
tree_last_fit <- 
  tree_final_wf %>%
  last_fit(splits)

# collect AUC and accuracy
fit <- tree_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "decision_tree") %>% 
  bind_rows(fit)

# extract final model and save it
tree_final_model <- tree_last_fit$.workflow[[1]]

# check if you get prediction out
predict(tree_final_model, data[222,])

# save model
saveRDS(tree_final_model, "./output/models/decision_tree.rds")


# KNN ---------------------------------------------------------------------

knn_spec <- 
  nearest_neighbor(neighbors = tune(), 
                   weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

# combine recipe and model to workflow
knn_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(knn_spec) 

# define search grid
knn_res <- tune_grid(
  knn_wf,
  resamples = folds,
  control = control_grid(parallel_over = "everything")
)

# Select Best Model 
knn_best_auc <- select_best(knn_res, "roc_auc")

# Finalize Model 
knn_final <- finalize_model(
  knn_spec,
  knn_best_auc
)

# final work flow and fit
knn_final_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(knn_final)

# last prediction on test data
knn_last_fit <- 
  knn_final_wf %>%
  last_fit(splits)

# collect AUC and accuracy
fit <- knn_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "knn") %>% 
  bind_rows(fit)

# extract final model and save it
knn_final_model <- knn_last_fit$.workflow[[1]]

# check if you get prediction out
predict(knn_final_model, data[222,])

# save model
saveRDS(knn_final_model, "./output/models/knn.rds")



# Naive Bayes -------------------------------------------------------------
naive_spec <- 
  naive_Bayes(smoothness = tune(), 
              Laplace = tune()) %>% 
  set_engine("klaR") 

# combine recipe and model to workflow
naive_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(naive_spec) 

# define search grid
naive_res <- tune_grid(
  naive_wf,
  resamples = folds,
  control = control_grid(parallel_over = "everything")
)


# Select Best Model 
naive_best_auc <- select_best(naive_res, "roc_auc")

# Finalize Model 
naive_final <- finalize_model(
  naive_spec,
  naive_best_auc
)

# final work flow and fit
naive_final_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(naive_final)

# last prediction on test data
naive_last_fit <- 
  naive_final_wf %>%
  last_fit(splits)

# collect AUC and accuracy
fit <- naive_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "naive_bayes") %>% 
  bind_rows(fit)


# extract final model and save it
naive_final_model <- naive_last_fit$.workflow[[1]]

# check if you get prediction out
predict(naive_final_model, data[222,])

# save model
saveRDS(naive_final_model, "./output/models/naive_bayes.rds")



# XGB Boost ---------------------------------------------------------------
xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(),                       ## randomness
  mtry = tune(),         
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# combine recipe and model to workflow
xgb_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(xgb_spec) 

# define search grid
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), 
           train),
  learn_rate(),
  size = 30
)

# tune grid
xgb_res <- tune_grid(
  xgb_wf,
  resamples = folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res$.notes[[1]]


# Select Best Model 
xgb_best_auc <- select_best(xgb_res, "roc_auc")

# Finalize Model 
xgb_final <- finalize_model(
  xgb_spec,
  xgb_best_auc
)

# final work flow and fit
xgb_final_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(xgb_final)

# last prediction on test data
xgb_last_fit <- 
  xgb_final_wf %>%
  last_fit(splits)

# collect AUC and accuracy
fit <- xgb_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "xgb_boost") %>% 
  bind_rows(fit)



