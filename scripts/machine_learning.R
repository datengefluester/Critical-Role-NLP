###############################################################################
# packages
###############################################################################
library(ggplot2)
# for sampling
library(dplyr)
# text mining
library(tm)
library(tidytext)
library(themis)
# extract additional class features
library(textfeatures)
# actual ML and dealing with feature engineering, class imbalance etc. 
library(tidymodels)
# for text feature in pipe
library(textrecipes) 
# for parallel computing
library(doParallel)


###############################################################################
# cloud vs. local
###############################################################################

# cloud
doParallel::registerDoParallel(cores = detectCores())
how_much_data <- 0.7


# local
doParallel::registerDoParallel(cores = detectCores() -1 )
how_much_data <- 0.01



###############################################################################
# read in data
###############################################################################


# read in all data frame clean data folder
file_list <- list.files("./data/clean_data/individual_episodes", pattern = "*.csv", full.names = TRUE)
data <- do.call(rbind,lapply(file_list,read.csv))

# drop the epilogues and everything, which is not the actual episodes
data <- data %>%
  filter(episode != 12) %>% 
  filter(episode <= 115) %>%
  filter(segment != "break") %>%
  filter(segment != "extra") %>% 
  filter(!is.na(segment))

# only keep relevant variables for machine learning
data <- data %>% select(episode, arc, turn_number,segment, actor_guest, text, time_in_sec, words_per_minute,rp_combat)

###############################################################################
# create dummy variable for Matt
###############################################################################
data <- data %>% 
  mutate(matt = if_else(actor_guest == "MATT", "Matt", "Other")) %>% 
  mutate(matt = as.factor(matt))

backup <- data

# set seed random: Mercer's birthday
set.seed(19820629)
data <- slice_sample(data, prop = 0.01)

###############################################################################
# silge
###############################################################################

# set = first episode airing
set.seed(20150312)

# splits
splits <- initial_split(data, strata = actor_guest)
train <- training(splits)
test <- testing(splits)

# seed for folds (airing first episode second campaign):
set.seed(20180111)
# cross validation folds
folds <- vfold_cv(train, strata = actor_guest)

cr_recipe <- recipe(actor_guest ~ text, data = train) %>% 
  step_downsample(actor_guest) %>% 
  step_textfeature(text) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

cr_prep <- prep(cr_recipe)
cr_juiced <- juice(cr_prep)


tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(tune_spec)

tune_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = 20
)



# by hand tuning to find best model
tmp <- tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) 

tmp %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


rf_grid <- grid_regular(
  min_n(range = c(20, 40)),
  mtry(range = c(2, 9)),
  levels = 5
)

regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

# select best
regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

# explore model: Variable importance
library(vip)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(actor_guest ~ .,
      data = juice(cr_prep) ) %>%
  vip(geom = "point")

# final work flow and fit
final_wf <- workflow() %>%
  add_recipe(cr_recipe) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(splits)

final_res %>%
  collect_metrics()



###############################################################################
# extract text features
###############################################################################

features <- textfeatures(data, sentiment = FALSE, 
                         word_dims = 0, normalize = FALSE)

features <-  bind_cols(data, features)





###############################################################################
# splits and folds
###############################################################################
# set seed for splits = first episode airing
set.seed(20150312)
splits <- initial_split(data, strata = matt)
train <- training(splits)
test <- testing(splits)

# seed for folds (airing first episode second campaign):
set.seed(20180111)
# cross validation folds
folds <- vfold_cv(train, strata = matt)


###############################################################################
# data pre processing
###############################################################################

# step_downsample: deal with class imbalance (matt speaks more often than the rest)
# step_textfeature: get the text feature function for ealier in a pipe
# step_zv: remove everything with zero variance
# step_normalize: normalize

cr_recipe <- recipe(matt ~ text, data = train) %>% 
  themis::step_downsample(matt) %>% 
  step_textfeature(text) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

cr_prep <- prep(cr_recipe)
cr_juiced <- juice(cr_prep)

###############################################################################
# models
###############################################################################

# random forest
rf_spec <- rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")


rf_spec_new <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")



# workflow
cr_workflow <- workflow() %>% 
  add_recipe(cr_recipe) 

###############################################################################
# actual fitting:
###############################################################################

# resamples = cross validation
# metrics = the fitting value to look at
# control = save predictions
# collect_metrics = get results
# conf_mat_resampled = confusion matrix


# random forest
rf_results <- cr_workflow %>% 
  add_model(rf_spec_new) %>% 
  fit_resamples(
    resamples = folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )
collect_metrics(rf_results) 
conf_mat_resampled(rf_results)
  

  
  






###############################################################################
# Evaluate models  
###############################################################################

knn_res <- fit_resamples(
  children ~ .,
  knn_spec,
  validation_splits,
  control = control_resamples(save_pred = TRUE)
)

knn_res %>%
  collect_metrics()












# support vector machine
svm_spec <- svm_rbf(cost = 0.5) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")


# support vector machine
svm_results <- cr_workflow %>% 
  add_model(svm_spec) %>% 
  fit_resamples(
    resamples = folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )
collect_metrics(svm_results) 
conf_mat_resampled(svm_results)  











###############################################################################
# tokenize and remove stopwords and punctuation
###############################################################################
# tokenize text
tokenized <- data %>%
  unnest_tokens(word, text)
# Remove english common stopwords
tokenized$word <- removeWords(tokenized$word, stopwords("english"))
# Remove punctuation
tokenized$word <- removePunctuation(tokenized$word)
# Eliminate extra white spaces
tokenized$word <- stripWhitespace(tokenized$word)
# Drop rows, which contained words, which were dropped in previous steps
tokenized <- tokenized %>% filter(word != "")


# and look into lime and parsnip
# https://www.youtube.com/watch?v=hAMjhbPJTkA
# https://www.youtube.com/watch?v=2Zcwa7HPg5w
# https://www.youtube.com/watch?v=ae_XVhjHd_o
# https://www.youtube.com/channel/UCTTBgWyJl2HrrhQOOc710kA

# https://www.r-bloggers.com/2021/02/machine-learning-with-r-a-complete-guide-to-decision-trees/
# https://www.r-bloggers.com/2021/02/predicting-housing-prices-with-natural-language-processing-nlp-and-tidymodels/















# topic modeling

tmp <- data %>% 
  unnest_tokens(word, Text)  %>% 
  anti_join(stop_words) %>% 
  filter(word !="yeah")

# kick out yeah?

tmp %>% 
  count(Actor, word, sort=TRUE) %>% 
  bind_tf_idf(Actor, word, n) %>% 
  group_by(Actor) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  
  

















# NOTE: renaming text variable!!!
Matt_data <- clean_data %>% select(c(Matt,Episode_number,startTime,endTime,text =Text_no_description,Arc,timeDiffInSecs,millisecsPerWord))
rm(list= ls()[!(ls() %in% c('Matt_data'))])
Matt_data$Matt <- as.factor(Matt_data$Matt)

set.seed(123)

cr_split <- initial_split(Matt_data, strata = Matt)
cr_train <- training(cr_split)
cr_test <- testing(cr_split)

set.seed(234)
cr_folds <- vfold_cv(cr_train, strata = Matt)
cr_folds



cr_recipe <- recipe(Matt ~ text, data = cr_train) %>%
  step_downsample(Matt) %>%
  step_textfeature(text) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())


cr_prep <- prep(cr_recipe)
cr_prep
juice(cr_prep)

# random forrest 
rf_spec <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_spec
# svm
svm_spec <- svm_rbf(cost = 0.5) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_spec

# workflow
cr_wf <- workflow() %>%
  add_recipe(cr_recipe)

cr_wf


# parallel computing
#library(parallel)
#no_cores <- detectCores() - 2  
#doParallel::registerDoParallel((cores=no_cores)) 

# random forest estimation
set.seed(1234)
rf_rs <- cr_wf %>%
  add_model(rf_spec) %>%
  fit_resamples(
    resamples = cr_folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_grid(save_pred = TRUE)
  )

collect_metrics(rf_rs)
conf_mat_resampled(rf_rs)


# svm
set.seed(2345)
svm_rs <- avatar_wf %>%
  add_model(svm_spec) %>%
  fit_resamples(
    resamples = avatar_folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_grid(save_pred = TRUE)
  )
collect_metrics(svm_rs)
conf_mat_resampled(svm_rs)


















