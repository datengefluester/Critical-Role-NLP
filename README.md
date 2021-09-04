Critical Role Subtitles
================

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
    ## │   │   ├── multinominal-regression
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
    ## │   ├── markdown_figs
    ## │   └── models
    ## └── scripts
    ##     └── individual_models

## Blog Texts

-   blog\_text.Rmd (or the markdown/html files)

## Script for Data Cleaning:

``` r
# run script
source("./scripts/data_cleaning.R", local = knitr::knit_global())
```

## Script for Creating the Data for the Visualizations

``` r
# run script
source("./scripts/data_descriptive_analysis.R", local = knitr::knit_global())
```

## Script for Machine Learning (work in progress!):

``` r
# run script
source("./scripts/machine_learning.R", local = knitr::knit_global())
```
