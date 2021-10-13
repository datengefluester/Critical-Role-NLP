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
tree_best_acc <- select_best(tree_res, "accuracy")

# save parameters
write.csv(tree_best_acc, "./data/machine_learning/decision-tree/decision_tree_parameters.csv", row.names = FALSE)

# Finalize Model
tree_final <- finalize_model(
  tree_spec,
  tree_best_acc
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
predict(tree_final_model, data[222, ])

# save model
saveRDS(tree_final_model, "./output/models/decision_tree.rds")

# get accuracy as data frame
tree_acc <- tree_last_fit %>%
  collect_metrics() %>%
  as.data.frame() %>%
  dplyr::select(.metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(model = "decision_tree") %>%
  dplyr::select(model, roc_auc, accuracy) %>%
  mutate(rightly_classified = accuracy * nrow(test)) %>%
  mutate(wrongly_classified = (1 - accuracy) * nrow(test))
write.csv(tree_acc, "./data/machine_learning/decision-tree/tree_acc.csv", row.names = FALSE)

# get predictions for graphs
predictions <- tree_last_fit %>%
  collect_predictions() %>%
  dplyr::select(-c(id, .config)) %>%
  mutate(model = "decision_tree") %>%
  relocate(model, .row, actor_guest, .pred_class)
write.csv(predictions, "./data/machine_learning/decision-tree/decision_tree_predictions.csv", row.names = FALSE)

#  vip graph
tree_fit_vip <- tree_final_wf %>%
  fit(test) %>%
  extract_fit_parsnip()

tree_vip_graph <- tree_fit_vip %>%
  vip(geom = "point")
save(tree_vip_graph, file = "./data/machine_learning/decision-tree/decision_tree_vip_graph")

# plotting the decision tree
png(
  file = "./data/machine_learning/decision-tree/decision_tree.png",
  width = 8192, height = 6144
)
rpart.plot(tree_fit$fit, type = 3, yesno = 2, clip.right.labs = FALSE, branch = .3, under = TRUE)
dev.off()


# drop what is no longer needed to save computational resources (but keep
# everything, which is needed for the other models)
drop <- ls()
drop <- drop[!drop %in% keep]
drop <- c(drop, "drop")
rm(list = drop)
