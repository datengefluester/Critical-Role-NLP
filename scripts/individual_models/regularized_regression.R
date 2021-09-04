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
regularized_best_acc <- select_best(regularized_res, "accuracy")

# save parameters
write.csv(regularized_best_acc, "./data/machine_learning/regularized-regression/regularized_parameters.csv", row.names = FALSE)

# Finalize Model 
regularized_final <- finalize_model(
  regularized_spec,
  regularized_best_acc
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
predict(regularized_final_model, data[222,])

# save model
saveRDS(regularized_final_model, "./output/models/regularized.rds")

# get accuracy as data frame
regularized_acc <- regularized_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "regularized") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) 
write.csv(regularized_acc, "./data/machine_learning/regularized-regression/regularized_acc.csv", row.names = FALSE)

# get predictions for graphs
predictions <- regularized_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "regularized") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(predictions, "./data/machine_learning/regularized-regression/regularized_predictions.csv", row.names = FALSE)

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)