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

# select best Model 
naive_best_acc <- select_best(naive_res, "accuracy")

# save parameters
write.csv(naive_best_acc, "./data/machine_learning/naive-bayes/naive_bayes_parameters.csv", row.names = FALSE)

# Finalize Model 
naive_final <- finalize_model(
  naive_spec,
  naive_best_acc
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

# get accuracy as data frame
naive_bayes_acc <- naive_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "naive_bayes") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) 

write.csv(naive_bayes_acc, "./data/machine_learning/naive-bayes/naive_bayes_acc.csv", row.names = FALSE)

# get predictions for graphs
predictions <- naive_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "naive_bayes") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(predictions, "./data/machine_learning/naive-bayes/naive_bayes_predictions.csv", row.names = FALSE)

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)