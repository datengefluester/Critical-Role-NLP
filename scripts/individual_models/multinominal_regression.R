set.seed(19820629)
# define model
multinom_spec <- 
  multinom_reg(penalty = NULL, 
               mixture = NULL) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

# combine recipe and model to workflow
multinom_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(multinom_spec) 

# define search grid and run model
multinom_res <- tune_grid(
  multinom_wf,
  resamples = folds,
  control = control_grid(parallel_over = "everything")
)

# select best model 
multinom_best_acc <- select_best(multinom_res, "accuracy")

# save parameters
write.csv(multinom_best_acc, "./data/machine_learning/multinominal/multinominal_parameters.csv", row.names = FALSE)

# finalize model 
multinom_final <- finalize_model(
  multinom_spec,
  multinom_best_acc
)

# final work flow and fit
multinom_final_wf <- 
  workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(multinom_final)

# last prediction on test data
multinom_last_fit <- 
  multinom_final_wf %>%
  last_fit(splits)

# extract final model and save it
multinom_final_model <- multinom_last_fit$.workflow[[1]]

# check if you get prediction out
predict(multinom_final_model, data[222,])

# save model
saveRDS(multinom_final_model, "./output/models/multinominal.rds")

# get accuracy as data frame
multinom_acc <- multinom_last_fit %>%
  collect_metrics() %>% 
  as.data.frame() %>% 
  dplyr::select(.metric, .estimate) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = "multinominal") %>% 
  dplyr::select(model, roc_auc, accuracy) %>% 
  mutate(rightly_classified = accuracy*nrow(test)) %>% 
  mutate(wrongly_classified = (1-accuracy)*nrow(test)) 
write.csv(multinom_acc, "./data/machine_learning/multinominal/multinominal_acc.csv", row.names = FALSE)

# get predictions for graphs
multinom_predictions <- multinom_last_fit %>%
  collect_predictions() %>% 
  dplyr::select(-c(id,.config)) %>% 
  mutate(model = "multinominal") %>% 
  relocate(model, .row, actor_guest, .pred_class)
write.csv(multinom_predictions, "./data/machine_learning/multinominal/multinom_predictions.csv", row.names = FALSE)

# drop what is no longer needed to save computational resources (but keep 
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop,"drop")
rm(list = drop)


