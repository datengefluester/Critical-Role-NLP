

# actual ML:

library(tidymodels)
library(textrecipes)
library(themis)
library(textfeatures)


###############################################################################
# How many words per Turn
###############################################################################


data %>% 
  ggplot(aes(wordCount)) + 
  geom_histogram(aes(y = stat(width*density)), fill ="#1b9e77", color="#3B3B3B") +
  scale_y_continuous(expand= c(0,0)) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,10,30,100,300,1000), trans="log1p", expand=c(0,0)) +
  labs(title = "Distribution Length of Turns",
       caption = "Source: Critical-Role-Subtitles") +
  bar_chart_theme() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=.2, color="#656565"),
        axis.line.x = element_line(size=.3, color="black"),
        axis.line.y = element_line(size=.3, color="black"))


# https://www.youtube.com/watch?v=ae_XVhjHd_o
# https://www.youtube.com/channel/UCTTBgWyJl2HrrhQOOc710kA


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


















