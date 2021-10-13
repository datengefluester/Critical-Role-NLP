# Load Packages -----------------------------------------------------------
library(ggplot2) # plotting
library(dplyr) # data manipulation
library(tidytext) # text mining
library(themis) # text mining
library(textfeatures) # extract additional class features
library(tidymodels) # actual ML and feature engineering, class imbalance etc.
library(textrecipes) # for text feature in pipe
library(doParallel) # for parallel computing
library(vip) # explore variable importance
library(rpart.plot) # plotting the decision tree
library(discrim) # for naive Bayes
library(data.table) # for faster reading in data


# define data frames to keep after each model -----------------------------
keep <- c(
  "cr_juiced", "cr_prep", "cr_recipe", "data",
  "fit_models", "folds", "splits", "test", "train", "keep"
)


# Read in Data ------------------------------------------------------------
file_list <- list.files("./data/clean_data/individual_episodes",
  pattern = "*.csv",
  full.names = TRUE
)

data <- do.call(rbind, lapply(file_list, fread)) %>% as.data.frame()

rm(file_list)

# drop the epilogues and everything, which is not the actual episodes
data <- data %>%
  filter(episode != 12) %>%
  filter(episode <= 115) %>%
  filter(segment != "break") %>%
  filter(segment != "extra") %>%
  filter(!is.na(segment)) %>%
  filter(!is.na(rp_combat)) %>%
  mutate(id = row_number())

# also drop every speaker, which contains 'AND' as these are multiple speakers,
# which increases the amount of classes to be predicted significantly and
# decrease the sample size by way too much to allow for a balanced sample
data <- data %>%
  filter(actor_guest != "ZAC") %>%
  filter(actor_guest != "BRIAN") %>%
  filter(actor_guest != "OFFSCREEN") %>%
  filter(!grepl("AND", actor_guest))

# make actor names not all caps
data <- data %>%
  mutate(actor_guest = stringr::str_to_title(actor_guest))

# only keep relevant variables for machine learning
data <- data %>% dplyr::select(
  id, actor_guest, episode, arc, segment,
  text, time_in_sec, words_per_minute, rp_combat
)

# transform rp_combat to be more intuitive
data <- data %>%
  rename(combat = rp_combat) %>%
  mutate(combat = replace(combat, combat == "role_play", 0)) %>%
  mutate(combat = replace(combat, combat == "combat", 1)) %>%
  mutate(combat = as.numeric(combat))

example_ml <- data %>%
  filter(episode == 100) %>%
  slice(1) %>%
  select(-c(id, episode))
write.csv(example_ml, "./data/machine_learning/example_ml/example_ml.csv", row.names = FALSE)

# Cloud vs. Local Machine -------------------------------------------------

# cloud
doParallel::registerDoParallel(cores = detectCores() - 1)

# local
# data <- data %>% dplyr::slice(1:10000)

# Splitting into Training, Testing and Folds ------------------------------

# set = first episode airing
set.seed(20150312)

# splits
splits <- initial_split(data, strata = actor_guest)
train <- training(splits)
test <- testing(splits)

# cross validation folds
# seed for folds (airing first episode second campaign):
set.seed(20180111)
folds <- vfold_cv(train, strata = actor_guest)


# Recipe ------------------------------------------------------------------
cr_recipe <- recipe(actor_guest ~ text + time_in_sec + words_per_minute +
  segment + combat, data = train) %>%
  step_downsample(actor_guest) %>% # deal with class imbalance
  step_dummy(segment) %>% # convert factor variables into multiple dummy variables
  step_mutate(text_copy = text) %>% # make a copy of the text
  step_textfeature(text_copy) %>% # extract text features from text
  step_normalize(
    time_in_sec,
    words_per_minute,
    contains("textfeature")
  ) %>% # normalize numeric predictors
  step_zv(all_predictors()) %>% # remove everything with zero variance
  step_lincomb(all_numeric()) %>% # remove any linear combinations
  step_tokenize(text) %>% # Tokenizes to words by default
  step_stopwords(text) %>% # Uses the english snowball list by default
  step_ngram(text,
    num_tokens = 3,
    min_num_tokens = 1
  ) %>% # extract 1grams, 2grams and 3grams
  step_tokenfilter(text,
    max_tokens = 300,
    min_times = 1
  ) %>% # keep 300 most useful tokens by tf-idf
  step_tfidf(text) %>%
  step_integer(all_predictors()) # make everything to numbers

summary(cr_recipe)


# Prep and Juice the data (processioning and finalizing data)

cr_prep <- prep(cr_recipe) # compute recipe
cr_juiced <- juice(cr_prep) # get pre-processed data


# 1. K-Nearest Neighbors --------------------------------------------------
source("./scripts/individual_models/knn.R")

# 2. Naive Bayes ----------------------------------------------------------
source("./scripts/individual_models/naive_bayes.R")

# 3. Decision Tree --------------------------------------------------------
# Note: the error comes from the image of the tree
# but it yields the desired results
source("./scripts/individual_models/decision_tree.R")

# 4. Random Forest --------------------------------------------------------
source("./scripts/individual_models/random_forest.R")

# 5. XGBoost --------------------------------------------------------------
source("./scripts/individual_models/xgboost.R")

# 6. Multinominal Regression ----------------------------------------------
source("./scripts/individual_models/multinominal_regression.R")

# 7. (Regularized) Regression ---------------------------------------------
source("./scripts/individual_models/regularized_regression.R")

# 8. Tuned Words XGboost --------------------------------------------------
# Note: this includes a separate recipe to incorporate
# number of words as another tuning parameter.
# Additionally, this only works using the full data set!
source("./scripts/individual_models/xg_tune_words.R")
