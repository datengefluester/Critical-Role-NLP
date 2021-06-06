Critical Role Subtitles
================

    ## .
    ## ├── data
    ## │   ├── clean_data
    ## │   │   ├── clean
    ## │   │   ├── dice_rolls
    ## │   │   ├── individual_episodes
    ## │   │   └── rest
    ## │   ├── data_for_graphs
    ## │   └── raw_data
    ## │       ├── dice_rolls
    ## │       ├── raw_csv
    ## │       ├── rest
    ## │       ├── srt_files
    ## │       └── time_stamps
    ## ├── output
    ## │   ├── images
    ## │   └── markdown_figs
    ## └── scripts

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

## Script for Machine Learning (work in progress):

``` r
# run script
source("./scripts/machine_learning.R", local = knitr::knit_global())
```
