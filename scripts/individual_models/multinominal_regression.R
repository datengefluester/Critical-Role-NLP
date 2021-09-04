# Run multinominal Regression
  
# Run multinominal Regression data:
# Recipe ------------------------------------------------------------------
train_multinominal <- train
  
cr_recipe_multinominal <- recipe(actor_guest ~ text +time_in_sec + words_per_minute + 
                            segment + combat, data = train_multinominal) %>% 
    step_downsample(actor_guest) %>% # deal with class imbalance
    step_dummy(segment) %>% # convert factor variables into many dummy variables
    step_mutate(text_copy = text) %>% # make a copy of the text
    step_textfeature(text_copy) %>%   # extract text features from text
    step_normalize(time_in_sec, 
                   words_per_minute, 
                   contains("textfeature"))  %>% # normalize numeric predictors
    step_zv(all_predictors())  %>%        # remove everything with zero variance
    step_lincomb(all_numeric())  %>%        # remove any linear combinations
    step_tokenize(text) %>% # Tokenizes to words by default
    step_stopwords(text) %>% # Uses the english snowball list by default
    step_ngram(text, num_tokens = 3, min_num_tokens = 1) %>%
    step_tokenfilter(text, max_tokens = 300, min_times = 1) %>%
    step_tfidf(text) %>% 
    step_integer(all_predictors()) 
  
# Prep and Juice the data (processioning and finalizing data) ------------
  
cr_prep_multinominal <- prep(cr_recipe_multinominal)    # compute recipe
cr_juiced_multinominal <- juice(cr_prep_multinominal)   # get pre-processed data


# Run Multinominal Model
multinomModel <- multinom(actor_guest ~ ., data=cr_juiced_multinominal, MaxNWts =10000000) 

# prepare data for prediction
test_multinominal <- test
cr_recipe_multinominal_test <- recipe(actor_guest ~ text +time_in_sec + words_per_minute + 
                                   segment + combat, data = test_multinominal) %>% 
  step_downsample(actor_guest) %>% # deal with class imbalance
  step_dummy(segment) %>% # convert factor variables into many dummy variables
  step_mutate(text_copy = text) %>% # make a copy of the text
  step_textfeature(text_copy) %>%   # extract text features from text
  step_normalize(time_in_sec, 
                 words_per_minute, 
                 contains("textfeature"))  %>% # normalize numeric predictors
  #step_zv(all_predictors())  %>%        # remove everything with zero variance
  step_lincomb(all_numeric())  %>%        # remove any linear combinations
  step_tokenize(text) %>% # Tokenizes to words by default
  step_stopwords(text) %>% # Uses the english snowball list by default
  step_ngram(text, num_tokens = 3, min_num_tokens = 1) %>%
  step_tokenfilter(text, max_tokens = 300, min_times = 1) %>%
  step_tfidf(text) %>% 
  step_integer(all_predictors()) 

# Prep and Juice the data (processioning and finalizing data) ------------
cr_prep_multinominal_test <- prep(cr_recipe_multinominal_test)    # compute recipe
cr_juiced_multinominal_test <- juice(cr_prep_multinominal_test)   # get pre-processed data

# ---- issue maybe textfeature_text_copy_first_person???

# Predict Test Data
predicted_scores <- predict (multinomModel, cr_juiced_multinominal_test, "probs") # predict on new data
