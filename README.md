Critical Role Subtitles
================

## Script for Data Cleaning:

``` r
source("./scripts/data_cleaning.R", local = knitr::knit_global())
```

## Script for Creating the Data for the Visualizations

``` r
source("./scripts/data_descriptive_analysis.R", local = knitr::knit_global())
```

## Script for Machine Learning:

Please note: the final models are too big for Github. So please drop me
a message, if you like to have them.

``` r
source("./scripts/machine_learning.R", local = knitr::knit_global())
```

# File Structure

    ## .
    ## ├── blog_posts
    ## │   └── markdown_figs
    ## ├── data
    ## │   ├── clean_data
    ## │   │   ├── dice_rolls
    ## │   │   ├── individual_episodes
    ## │   │   └── rest
    ## │   ├── descriptive_analysis
    ## │   ├── machine_learning
    ## │   │   ├── decision-tree
    ## │   │   ├── example_ml
    ## │   │   ├── k-nearest-neighbors
    ## │   │   ├── naive-bayes
    ## │   │   ├── random-forest
    ## │   │   ├── regularized-regression
    ## │   │   ├── xg-tune-words
    ## │   │   └── xgboost
    ## │   ├── machine_learning_backup
    ## │   │   ├── decision-tree
    ## │   │   ├── example_ml
    ## │   │   ├── k-nearest-neighbors
    ## │   │   ├── multinominal-regression
    ## │   │   ├── naive-bayes
    ## │   │   ├── random-forest
    ## │   │   ├── regularized-regression
    ## │   │   ├── xg-tune-words
    ## │   │   └── xgboost
    ## │   └── raw_data
    ## │       ├── dice_rolls
    ## │       ├── raw_csv
    ## │       ├── rest
    ## │       ├── srt_files
    ## │       └── time_stamps
    ## ├── output
    ## │   ├── images
    ## │   │   ├── descriptive_statistics
    ## │   │   └── machine_learning
    ## │   └── models
    ## └── scripts
    ##     └── individual_models
