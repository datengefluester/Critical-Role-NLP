# Recipe ------------------------------------------------------------------
tune_cr_recipe <- recipe(actor_guest ~ text +time_in_sec + words_per_minute + 
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
  step_tokenfilter(text, max_tokens = tune(), min_times = 1) %>%
  step_tfidf(text) %>% 
  step_integer(all_predictors()) 


# XGBoost tuned words-----------------------------------------------------------------
set.seed(19820629)
# define model
xgb_tune_spec <- boost_tree(
  trees = tune(), 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# combine recipe and model to workflow
xgb_tune_wf <- workflow() %>%
  add_recipe(tune_cr_recipe) %>%
  add_model(xgb_tune_spec) 

# define search grid
xgb_tune_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  max_tokens(),
  trees(),
  finalize(mtry(), 
           train),
  learn_rate(),
  size = 100
)

# tune grid
xgb_tune_res <- tune_grid(
  xgb_tune_wf,
  resamples = folds,
  grid = xgb_tune_grid,
  control = control_grid(save_pred = TRUE)
)

# Select Best Model 
xgb_tune_best_acc <- select_best(xgb_tune_res, "accuracy")

# save parameters
write.csv(xgb_tune_best_acc, "./data/machine_learning/xg-tune-words/xgb_tune_parameters.csv", row.names = FALSE)

# Finalize Model 
xgb_tune_final <- finalize_model(
  xgb_tune_spec,
  xgb_tune_best_acc
)

# final work flow and fit
xgb_tune_final_wf <- 
  workflow() %>%
  add_recipe(tune_cr_recipe) %>%
  add_model(xgb_tune_final)

# last prediction on test data
xgb_tune_last_fit <- 
  xgb_tune_final_wf %>%
  last_fit(splits)

# extract final model and save it
xgb_tune_final_model <- xgb_tune_last_fit$.workflow[[1]]

# check if you get prediction out
predict(xgb_tune_final_model, data[222,])

# save model
saveRDS(xgb_tune_final_model, "./output/models/xgboost_tune.rds")

# get accuracy as data frame
xgb_tune_acc <- xgb_tune_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "xbg_boost_tune") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) 
write.csv(xgb_tune_acc, "./data/machine_learning/xg-tune-words/xgb_tune_acc.csv", row.names = FALSE)

# get predictions for graphs
xgb_tune_predictions <- xgb_tune_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "xbg_boost_tune") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(xgb_tune_predictions, "./data/machine_learning/xg-tune-words/xgb_tune_predictions.csv", row.names = FALSE)

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)