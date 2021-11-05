Analysis of Critical Role Season 1 Subtitles
================

### Script for Data Cleaning

``` r
source("./scripts/data_cleaning.R", local = knitr::knit_global())
```

### Script for Creating the Data for the Visualizations

``` r
source("./scripts/data_descriptive_analysis.R", local = knitr::knit_global())
```

### Script for Machine Learning

``` r
source("./scripts/machine_learning.R", local = knitr::knit_global())
```

### Final models can be downloaded from

<https://filen.io/f/f989e51e-e34e-400e-bfa5-1a298f9a50d0#!xzQRJOQQFFfrnMdmwAlvCGRyuUsd7WsJ>

## Links to Blog Posts

### Descriptive Analysis

<https://datengefluester.de/critical-graphs-shenanigans-with-critical-role-season-1-subtitles/>

### Machine Learning: Data Preparation

<https://datengefluester.de/critical-machine-learning-predicting-critical-role-actors-from-text-part-1/>

### Machine Learning: Training Models

<https://datengefluester.de/critical-machine-learning-predicting-critical-role-actors-from-text-part-2/>

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
    ## │   │   ├── multinominal_regression
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
