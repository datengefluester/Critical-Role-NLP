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
  metrics = metric_set(accuracy),
  control = control_grid(parallel_over = "everything", save_pred = TRUE), # parallel tuning
)

# Select Best Model
rf_best_acc <- select_best(rf_res, "accuracy")

# save parameters
write.csv(rf_best_acc, "./data/machine_learning/random-forest/rf_parameters.csv", row.names = FALSE)

# Finalize Model 
rf_final <- finalize_model(
  rf_spec,
  rf_best_acc
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
             accuracy,roc_auc))

# extract final model and save it
rf_final_model <- rf_last_fit$.workflow[[1]]

# check if you get prediction out
predict(rf_final_model, data[222,])

# save model
saveRDS(rf_final_model, "./output/models/rf.rds")

# get accuracy as data frame
rf_acc <- rf_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "random_forest") %>% 
  relocate(model) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) 
write.csv(rf_acc, "./data/machine_learning/random-forest/rf_acc.csv", row.names = FALSE)

# get predictions for graphs
rf_predictions <- rf_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "random_forest") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(rf_predictions, "./data/machine_learning/random-forest/rf_predictions.csv", row.names = FALSE)

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)
