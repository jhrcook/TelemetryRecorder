Hyperparameter tuning of the Random Forest Classifier
================

The purpose of this notebook is the comprehensively tune the
hyperparameters of a Random Forest (RF) Classifier for detecting states
of a push-up. The [*Hidden Markov Model-based learning
pipelines*](05_008_hmm_pipelines.md) notebook demonstrated that a RF is
likely the best model for this task, though did not pursue full
optimization of the classifier.

Data
----

The data used here was cached in [*Hidden Markov Model-based learning
pipelines*](05_008_hmm_pipelines.md).

    processed_pushup_data <- readRDS(
      here(file.path("cache", "hmm_processed_pushup_data.rds"))
    )
    processed_pushup_data

    #> # A tibble: 6 x 10
    #>   workout_idx exercise reps  date                data  wide_data model fit  
    #>         <int> <chr>    <chr> <dttm>              <lis> <list>    <lis> <lis>
    #> 1           1 Push-Ups 10    2020-10-03 17:43:59 <tib… <tibble … <dep… <dpm…
    #> 2           2 Push-Ups 10    2020-10-04 13:24:29 <tib… <tibble … <dep… <dpm…
    #> 3           3 Push-Ups 10    2020-10-04 13:25:57 <tib… <tibble … <dep… <dpm…
    #> 4           4 Push-Ups 10    2020-10-05 12:36:09 <tib… <tibble … <dep… <dpm…
    #> 5           5 Push-Ups 10    2020-10-05 12:36:49 <tib… <tibble … <dep… <dpm…
    #> 6           6 Push-Ups 10    2020-10-11 08:43:16 <tib… <tibble … <dep… <dpm…
    #> # … with 2 more variables: full_data <list>, classifier_data <list>

Hyperparameter tuning
---------------------

**To-Do**: learn about the tuning process available in TidyModels
