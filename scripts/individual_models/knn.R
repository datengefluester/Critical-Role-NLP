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

# define search grid and run model
knn_res <- tune_grid(
  knn_wf,
  resamples = folds,
  control = control_grid(parallel_over = "everything")
)

# select best model 
knn_best_acc <- select_best(knn_res, "accuracy")

# save parameters
write.csv(knn_best_acc, "./data/machine_learning/k-nearest-neighbors/knn_parameters.csv", row.names = FALSE)

# finalize model 
knn_final <- finalize_model(
  knn_spec,
  knn_best_acc
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

# get accuracy as data frame
knn_acc <- knn_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "knn") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) 
write.csv(knn_acc, "./data/machine_learning/k-nearest-neighbors/knn_acc.csv", row.names = FALSE)

# get predictions for graphs
predictions <- knn_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "knn") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(predictions, "./data/machine_learning/k-nearest-neighbors/knn_predictions.csv", row.names = FALSE)

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)