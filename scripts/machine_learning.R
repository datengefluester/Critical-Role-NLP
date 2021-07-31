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
library(rpart.plot)   # plotting the decision tree
library(discrim)      # for naive Bayes
library(data.table)   # for faster reading in data

# define data frames to keep after each model -----------------------------
keep <- c("cr_juiced", "cr_prep", "cr_recipe", "data",
          "fit_models", "folds", "splits", "test", "train", "keep")


# Read in Data ------------------------------------------------------------
file_list <- list.files("./data/clean_data/individual_episodes", 
                        pattern = "*.csv", 
                        full.names = TRUE)

data <- do.call(rbind,lapply(file_list,fread)) %>% as.data.frame()

rm(file_list)

# drop the epilogues and everything, which is not the actual episodes
data <- data %>%
  filter(episode != 12) %>% 
  filter(episode <= 115) %>%
  filter(segment != "break") %>%
  filter(segment != "extra") %>% 
  filter(!is.na(segment)) %>% 
  filter(!is.na(rp_combat)) %>% 
  mutate(id = row_number())

# also drop every speaker, which contains 'AND' as these are multiple speakers,
# which increases the amount of classes to be predicted significantly and 
# decrease the sample size by way too much to allow for a balanced sample
data <- data %>% 
  filter(actor_guest != "ZAC") %>%
  filter(actor_guest != "BRIAN") %>%
  filter(actor_guest != "OFFSCREEN") %>% 
  filter(!grepl("AND", actor_guest))

# make actor names not all caps
data <- data %>%
  mutate(actor_guest = stringr::str_to_title(actor_guest))

# only keep relevant variables for machine learning
data <- data %>% dplyr::select(id, actor_guest, episode, arc, segment, 
                        text, time_in_sec, words_per_minute, rp_combat)

# transform rp_combat to be more intuitive
data <- data %>% 
  rename(combat = rp_combat) %>% 
  mutate(combat = replace(combat, combat=="role_play", 0)) %>% 
  mutate(combat = replace(combat, combat=="combat", 1)) %>% 
  mutate(combat = as.numeric(combat))

example_ml <- data %>% 
  filter(episode == 100) %>% 
  slice(1) %>% 
  select(-c(id,episode)) 
write.csv(example_ml, "./data/machine_learning/example_ml.csv", row.names = FALSE)

# Cloud vs. Local Machine -------------------------------------------------

# cloud
doParallel::registerDoParallel(cores = detectCores()-1)


# local
data <- data %>% dplyr::slice(1:10000)

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
cr_recipe <- recipe(actor_guest ~ text +time_in_sec + words_per_minute + 
                      segment + combat, data = train) %>% 
  step_downsample(actor_guest) %>% # deal with class imbalance
  step_dummy(segment) %>% # convert factor variables into multiple dummy variables
  step_mutate(text_copy = text) %>% # make a copy of the text
  step_textfeature(text_copy) %>%   # extract text features from text
  step_normalize(time_in_sec, words_per_minute, contains("textfeature"))  %>% # normalize numeric predictors
  step_zv(all_predictors())  %>%           # remove everything with zero variance
  step_lincomb(all_numeric())  %>%        # remove any linear combinations
  step_tokenize(text) %>% # Tokenizes to words by default
  step_stopwords(text) %>% # Uses the english snowball list by default
  step_ngram(text, num_tokens = 3, min_num_tokens = 1) %>%
  step_tokenfilter(text, max_tokens = 300, min_times = 1) %>%
  step_tfidf(text) %>% 
  step_integer(all_predictors()) 

summary(cr_recipe)


# Prep and Juice the data (processioning and finalizing data) ------------

 cr_prep <- prep(cr_recipe)    # compute recipe
 cr_juiced <- juice(cr_prep)   # get pre-processed data


# Random Forrest ----------------------------------------------------------

set.seed(19820629)
# define it model
rf_spec <- rand_forest(
  mtry = tune(),        # number of predictors randomly sampled
  trees = tune(),         # numbers trees fitted
  min_n = tune()        # minimum number of data points in a node
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# combine recipe and model to workflow
rf_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(rf_spec) 


# run model
rf_res <- tune_grid(
  rf_wf,
  resamples = folds,
  metrics = metric_set(
    recall, precision, f_meas, 
    accuracy, kap,
    roc_auc, sens, spec),
  control = control_grid(parallel_over = "everything", save_pred = TRUE), # parallel tuning
)


# Find Best Model: Plotting 
rf_res %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  dplyr::select(mean, min_n, mtry, trees, max_tokens) %>%
  pivot_longer(min_n:trees,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "accuracy")


# Define new Grid by Hand and Rerun Model

# new grid based on previous results
rf_grid <- grid_regular(
  min_n(range = c(18, 45)),
  mtry(range = c(1, 15)),
  trees(range = c(1500,1800)),
  levels = 1
)

# rerun model
rf_regular <- tune_grid(
  rf_wf,
  resamples = folds,
  grid = rf_grid,
  metrics = metric_set(
    recall, precision, f_meas, 
    accuracy, kap,
    roc_auc, sens, spec),
  control = control_grid(parallel_over = "everything", save_pred = TRUE),
)


# Select Best Model
rf_best_auc <- select_best(rf_regular, "accuracy")

# Finalize Model 
rf_final <- finalize_model(
  rf_spec,
  rf_best_auc
)

# final work flow and fit
rf_final_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(rf_final)

# last prediction on test data
rf_last_fit <- 
  rf_final_wf %>%
  last_fit(split = splits,
           metrics = metric_set(
             recall, precision, f_meas, 
             accuracy, kap,
             roc_auc, sens, spec))

# collect AUC and accuracy
rf_last_fit %>%
  collect_metrics()

# extract final model and save it
rf_final_model <- rf_last_fit$.workflow[[1]]

# check if you get prediction out
predict(rf_final_model, data[222,])

# save model
saveRDS(rf_final_model, "./output/models/rf.rds")

# create data frame for overview for all models
fit_models <- read.csv("./data/machine_learning/fit_models.csv")

fit_models <- rf_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "random_forest") %>% 
  relocate(model) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) %>% 
  bind_rows(fit_models) %>% 
  group_by(model) %>%
  filter(row_number(model) == 1)
write.csv(fit_models, "./data/machine_learning/fit_models.csv", row.names = FALSE)

# get predictions for graphs
predictions <- rf_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "random_forest") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(predictions, "./data/machine_learning/rf_predictions.csv", row.names = FALSE)

# Explore Model: Variable Importance 
rf_vip_graph <- rf_final %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(actor_guest ~ .,
      data = juice(cr_prep) ) %>%
  vip(geom = "point")
save(rf_vip_graph, file = "./data/machine_learning/rf_vip_graph")

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)

# SVM ---------------------------------------------------------------------
set.seed(19820629)
# define SVM
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

# extract final model and save it
svm_final_model <- svm_last_fit$.workflow[[1]]

# check if you get prediction out
predict(svm_final_model, data[222,])

# save model
saveRDS(svm_final_model, "./output/models/svm.rds")

# add to data frame for overview
fit_models <- read.csv("./data/machine_learning/fit_models.csv")

fit_models <- svm_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "svm") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) %>% 
  bind_rows(fit_models) %>% 
  group_by(model) %>%
  filter(row_number(model) == 1)
write.csv(fit_models, "./data/machine_learning/fit_models.csv", row.names = FALSE)

# get predictions for graphs
predictions <- svm_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "svm") %>% 
  relocate(model, .row, actor_guest, .pred_class)

write.csv(predictions, "./data/machine_learning/svm_predictions.csv", row.names = FALSE)


# Explore Model: Variable Importance 
# https://stackoverflow.com/questions/62772397/integration-of-variable-importance-plots-within-the-tidy-modelling-framework

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)


# Decision Tree -----------------------------------------------------------
set.seed(19820629)
# define model
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

# extract final model and save it
tree_final_model <- tree_last_fit$.workflow[[1]]

# check if you get prediction out
predict(tree_final_model, data[222,])

# save model
saveRDS(tree_final_model, "./output/models/decision_tree.rds")

# add to data frame for overview
fit_models <- read.csv("./data/machine_learning/fit_models.csv")
fit_models <- tree_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "decision_tree") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) %>% 
  bind_rows(fit_models) %>% 
  group_by(model) %>%
  filter(row_number(model) == 1)
write.csv(fit_models, "./data/machine_learning/fit_models.csv", row.names = FALSE)

# get predictions for graphs
predictions <- tree_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "decision_tree") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(predictions, "./data/machine_learning/decision_tree_predictions.csv", row.names = FALSE)

#  vip graph
tree_fit <- tree_final_wf %>%
  fit(test) %>% 
  pull_workflow_fit()

tree_vip_graph <- tree_fit %>%
  vip(geom = "point") 
save(tree_vip_graph, file = "./data/machine_learning/decision_tree_vip_graph")

# plotting the decision tree
png(file="./data/machine_learning/decision_tree.png",
    width = 2048, height = 1536)
rpart.plot(tree_fit$fit, type = 3, yesno=2, clip.right.labs = FALSE, branch = .3, under = TRUE)
dev.off()


# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)



# KNN ---------------------------------------------------------------------
set.seed(19820629)
# define model
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
knn_best_auc <- select_best(knn_res, "accuracy")

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

# extract final model and save it
knn_final_model <- knn_last_fit$.workflow[[1]]

# check if you get prediction out
predict(knn_final_model, data[222,])

# save model
saveRDS(knn_final_model, "./output/models/knn.rds")

# add to data frame for overview
fit_models <- read.csv("./data/machine_learning/fit_models.csv")
fit_models <- knn_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "knn") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) %>% 
  bind_rows(fit_models) %>% 
  group_by(model) %>%
  filter(row_number(model) == 1)
write.csv(fit_models, "./data/machine_learning/fit_models.csv", row.names = FALSE)

# get predictions for graphs
predictions <- knn_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "knn") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(predictions, "./data/machine_learning/knn_predictions.csv", row.names = FALSE)

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)

# Naive Bayes -------------------------------------------------------------
set.seed(19820629)
# define model
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

# extract final model and save it
naive_final_model <- naive_last_fit$.workflow[[1]]

# check if you get prediction out
predict(naive_final_model, data[222,])

# save model
saveRDS(naive_final_model, "./output/models/naive_bayes.rds")

# add to data frame for overview
fit_models <- read.csv("./data/machine_learning/fit_models.csv")
fit_models <- naive_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "naive_bayes") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) %>% 
  bind_rows(fit_models) %>% 
  group_by(model) %>%
  filter(row_number(model) == 1)
write.csv(fit_models, "./data/machine_learning/fit_models.csv", row.names = FALSE)

# get predictions for graphs
predictions <- naive_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "naive_bayes") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(predictions, "./data/machine_learning/naive_bayes_predictions.csv", row.names = FALSE)

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)

# XGBoost -----------------------------------------------------------------
set.seed(19820629)
# define model
xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
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

# extract final model and save it
xgb_final_model <- xgb_last_fit$.workflow[[1]]

# check if you get prediction out
predict(xgb_final_model, data[222,])

# save model
saveRDS(xgb_final_model, "./output/models/xgboost.rds")

# add to data frame for overview
fit_models <- read.csv("./data/machine_learning/fit_models.csv")
fit_models <- xgb_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "xbg_boost") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) %>% 
  bind_rows(fit_models) %>% 
  group_by(model) %>%
  filter(row_number(model) == 1)
write.csv(fit_models, "./data/machine_learning/fit_models.csv", row.names = FALSE)

# get predictions for graphs
predictions <- xgb_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "xbg_boost") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(predictions, "./data/machine_learning/xbg_boost_predictions.csv", row.names = FALSE)

# vip plot
# Explore Model: Variable Importance 
xgb_vip_graph <- xgb_final_wf %>% 
  fit(data = test) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")
save(xgb_vip_graph, file = "./data/machine_learning/xbg_boost_vip_graph")

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)


# Regularized Regression --------------------------------------------------
set.seed(19820629)
# define model
regularized_spec <- multinom_reg(
  penalty = tune(), 
  mixture = tune()) %>%
  set_engine("glmnet") %>% 
  set_mode("classification")

# combine recipe and model to workflow
regularized_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(regularized_spec) 

# tune grid
regularized_res <- tune_grid(
  regularized_wf,
  resamples = folds,
  control = control_grid(parallel_over = "everything"), # parallel tuning
)

# Select Best Model 
regularized_best_auc <- select_best(regularized_res, "roc_auc")

# Finalize Model 
regularized_final <- finalize_model(
  regularized_spec,
  regularized_best_auc
)

# final work flow and fit
regularized_final_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(regularized_final)

# last prediction on test data
regularized_last_fit <- 
  regularized_final_wf %>%
  last_fit(splits)

# extract final model and save it
regularized_final_model <- regularized_last_fit$.workflow[[1]]

# check if you get prediction out
tidy(extract_model(regularized_last_fit$.workflow[[1]]))

predict(regularized_final_model, data[222,])

# save model
saveRDS(regularized_final_model, "./output/models/regularized.rds")


# add to data frame for overview
fit_models <- read.csv("./data/machine_learning/fit_models.csv")
fit_models <- regularized_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "regularized") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) %>% 
  bind_rows(fit_models) %>% 
  group_by(model) %>%
  filter(row_number(model) == 1)
write.csv(fit_models, "./data/machine_learning/fit_models.csv", row.names = FALSE)

# get predictions for graphs
predictions <- regularized_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "lasso") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(predictions, "./data/machine_learning/regularized_predictions.csv", row.names = FALSE)

# Explore Model: Variable Importance 
regularized_model <- regularized_final_wf %>% 
  fit(data = test) %>%
  pull_workflow_fit() %>%
  tidy()
save(regularized_vip_graph, file = "./data/machine_learning/regularized_model")

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)

