set.seed(19820629)
# define model
xgboost_spec <- boost_tree(
  trees = tune(), 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# combine recipe and model to workflow
xgboost_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(xgboost_spec) 

# define search grid
xgboost_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  trees(),
  finalize(mtry(), 
           train),
  learn_rate(),
  size = 30
)

# tune grid
xgboost_res <- tune_grid(
  xgboost_wf,
  resamples = folds,
  grid = xgboost_grid,
  control = control_grid(save_pred = TRUE)
)

# Select Best Model 
xgboost_best_acc <- select_best(xgboost_res, "accuracy")

# save parameters
write.csv(xgboost_best_acc, "./data/machine_learning/xgboost/xgboost_parameters.csv", row.names = FALSE)

# Finalize Model 
xgboost_final <- finalize_model(
  xgboost_spec,
  xgboost_best_acc
)

# final work flow and fit
xgboost_final_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(xgboost_final)

# last prediction on test data
xgboost_last_fit <- 
  xgboost_final_wf %>%
  last_fit(splits)

# extract final model and save it
xgboost_final_model <- xgboost_last_fit$.workflow[[1]]

# check if you get prediction out
predict(xgboost_final_model, data[222,])

# save model
saveRDS(xgboost_final_model, "./output/models/xgboost.rds")

# get accuracy as data frame
xgboost_acc <- xgboost_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "xg_boost") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) 
write.csv(xgboost_acc, "./data/machine_learning/xgboost/xgboost_acc.csv", row.names = FALSE)

# get predictions for graphs
xgboost_predictions <- xgboost_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "xgboost_boost") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(xgboost_predictions, "./data/machine_learning/xgboost/xgboost_predictions.csv", row.names = FALSE)

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)