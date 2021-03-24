###############################################################################
# packages
###############################################################################
library(tm)
library(tidytext)

# extract additional class features
library(textfeatures)
# deal with, among other things, class imbalance
library(themis)
# actual ML
library(tidymodels)
# for text feature in pipe
library(textrecipes) 
# for parallel computing
library(doParallel)
doParallel::registerDoParallel(cores = detectCores() - 1)
###############################################################################
# read in data
###############################################################################

data <- read.csv("./data/clean_data/clean_ml.csv")


###############################################################################
# create dummy variable for Matt
###############################################################################
data <- data %>% mutate(matt = if_else(actor == "MATT", "Matt", "Other"))

###############################################################################
# extract text features
###############################################################################

features <- textfeatures(data, sentiment = FALSE, 
                     word_dims = 0, normalize = FALSE)

features <-  bind_cols(data, features)


###############################################################################
# splits and folds
###############################################################################
# set = first episode airing
set.seed(20150312)

# splits
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
  step_downsample(matt) %>% 
  step_textfeature(text) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

###############################################################################
# models
###############################################################################

# random forest
rf_spec <- rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# support vector machine
svm_spec <- svm_rbf(cost = 0.5) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

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
  add_model(rf_spec) %>% 
  fit_resamples(
    resamples = folds,
    metrics = metric_set(roc_auc, accuracy, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )
collect_metrics(rf_results) 
conf_mat_resampled(rf_results)
  
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


















