Hidden Markov Model-based learning pipelines
================

Below, I try out a HMM-based modeling and prediction pipeline.

Data
----

    zscale <- function(x, na.rm = TRUE) {
      (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
    }


    apply_scale_trans <- function(df, x = value, y = scaled_value) {
      df %.% {
        group_by(axis, motion)
        mutate({{ y }} := zscale({{ x }}))
        ungroup()
      }
    }


    apply_smoothing_trans <- function(df,
                                      x = value,
                                      y = smooth_value,
                                      rolling_n = 10) {
      before_after <- round(rolling_n / 2)

      df %.% {
        group_by(axis, motion)
        mutate(
          {{ y }} := slider::slide_dbl(
            {{ x }},
            .f = ~ max(abs(.x)),
            .before = before_after,
            .after = before_after
          ),
          {{ y }} := slider::slide_dbl(
            {{ y }},
            .f = mean,
            .before = 2,
            .after = 2
          )
        )
      }
    }


    transform_pushup_data <- function(df) {
      df %>%
        apply_scale_trans(
          x = value,
          y = scaled_value
        ) %>%
        apply_smoothing_trans(
          x = value,
          y = smooth_value
        ) %>%
        apply_smoothing_trans(
          x = scaled_value,
          y = scaled_smooth_value
        )
    }


    pushup_data <- tibble(filename = get_data_file_names(data_dir)) %.% {
      filter(str_detect(filename, "Push"))
      mutate(
        workout_idx = row_number(),
        all_data = map(filename, read_watch_data),
        file_info = map(all_data, ~ .x$meta_data),
        data = map(all_data, ~ .x$telemetry_data),
        data = map(data, transform_pushup_data),
        data = map(data, ~ rename(.x, time_step = date)),
        data = map(data, ~ select(
          .x,
          time_step, idx, motion, axis, 
          value, scaled_value, smooth_value, scaled_smooth_value
        ))
      )
      select(-all_data, -filename)
      unnest(file_info)
      select(workout_idx, exercise, reps, date, data)
    }

    pushup_data

    #> # A tibble: 6 x 5
    #>   workout_idx exercise reps  date                data                 
    #>         <int> <chr>    <chr> <dttm>              <list>               
    #> 1           1 Push-Ups 10    2020-10-03 17:43:59 <tibble [10,086 × 8]>
    #> 2           2 Push-Ups 10    2020-10-04 13:24:29 <tibble [9,288 × 8]> 
    #> 3           3 Push-Ups 10    2020-10-04 13:25:57 <tibble [11,082 × 8]>
    #> 4           4 Push-Ups 10    2020-10-05 12:36:09 <tibble [10,596 × 8]>
    #> 5           5 Push-Ups 10    2020-10-05 12:36:49 <tibble [11,898 × 8]>
    #> 6           6 Push-Ups 10    2020-10-11 08:43:16 <tibble [10,830 × 8]>

Overview
--------

**Pipeline**

1.  Chop the raw data within an IQR of the time steps to get just the
    clean push-up data.
2.  Use an HMM to identify the 2 states of the push-up.
3.  Use the HMM to cut the chopped data into the 3 states of a push-up
    (one state as `unknown`).
4.  Train a classifier on this training data.
5.  Apply the classifier to new data as it is being collected.

**Experimentation**

-   A lot of experimentation will be needed to select the best
    classification model and tune the model’s hyperparameters.

Pipeline
--------

### 1. Chop the data

Select only the time steps in the 30 and 70 percentiles.

    chopped_pushup_data <- pushup_data %.% {
      unnest(data)
      group_by(workout_idx)
      filter(time_step > quantile(time_step, 0.3) & time_step < quantile(time_step, 0.7))
      ungroup()
    }

    # Number of data points per workout.
    chopped_pushup_data %>%
      group_by(workout_idx) %>%
      summarise(n_datapoints = n_distinct(idx)) %>%
      ungroup()

    #> # A tibble: 6 x 2
    #>   workout_idx n_datapoints
    #>         <int>        <int>
    #> 1           1          671
    #> 2           2          618
    #> 3           3          737
    #> 4           4          706
    #> 5           5          793
    #> 6           6          721

    chopped_pushup_data %>%
      ggplot(aes(x = time_step, y = scaled_smooth_value)) +
      facet_wrap(~workout_idx, scales = "free", ncol = 2) +
      geom_line(aes(color = axis))

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### 2. Train HMM

    nest_pushup_exercises <- function(df) {
      df %>%
        group_by(workout_idx, exercise, reps, date) %>%
        nest() %>%
        ungroup()
    }


    pivot_telemetry_data <- function(telemetry_data, x = value) {
      telemetry_data %>%
        pivot_wider(
          c(time_step, idx),
          names_from = axis,
          values_from = {{ x }}
        )
    }

    construct_pushup_hmm <- function(d, nstates = 2) {
      depmix(
        list(
          x ~ 1,
          y ~ 1,
          z ~ 1,
          pitch ~ 1,
          roll ~ 1,
          yaw ~ 1
        ),
        data = d,
        nstates = nstates,
        family = list(
          gaussian(), gaussian(), gaussian(),
          gaussian(), gaussian(), gaussian()
        )
      )
    }



    chopped_pushup_hmms <- chopped_pushup_data %.% {
      nest_pushup_exercises()
      mutate(
        wide_data = map(data, pivot_telemetry_data, x = scaled_smooth_value),
        model = map(wide_data, construct_pushup_hmm),
        fit = map(model, fit)
      )
    }

    #> converged at iteration 47 with logLik: 935.1952 
    #> converged at iteration 15 with logLik: 2533.44 
    #> converged at iteration 19 with logLik: 1009.4 
    #> converged at iteration 20 with logLik: 2035.068 
    #> converged at iteration 105 with logLik: 1837.033 
    #> converged at iteration 15 with logLik: 1577.959

    chopped_pushup_hmms %>%
      mutate(a = walk2(data, fit, plot_hmm_fit, data_x = smooth_value))

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-5-6.png)<!-- -->

    #> # A tibble: 6 x 9
    #>   workout_idx exercise reps  date                data  wide_data model fit  
    #>         <int> <chr>    <chr> <dttm>              <lis> <list>    <lis> <lis>
    #> 1           1 Push-Ups 10    2020-10-03 17:43:59 <tib… <tibble … <dep… <dpm…
    #> 2           2 Push-Ups 10    2020-10-04 13:24:29 <tib… <tibble … <dep… <dpm…
    #> 3           3 Push-Ups 10    2020-10-04 13:25:57 <tib… <tibble … <dep… <dpm…
    #> 4           4 Push-Ups 10    2020-10-05 12:36:09 <tib… <tibble … <dep… <dpm…
    #> 5           5 Push-Ups 10    2020-10-05 12:36:49 <tib… <tibble … <dep… <dpm…
    #> 6           6 Push-Ups 10    2020-10-11 08:43:16 <tib… <tibble … <dep… <dpm…
    #> # … with 1 more variable: a <list>

For the purposes of this exploration of training processes, I need the
states of the HMMs to be synchronized. The vector
`DATASETS_TO_REVERSE_STATES` has the indices of fit models that need to
have their predicted states flipped to match the first model’s state
assignments.

    DATASETS_TO_REVERSE_STATES <- c(3, 5, 6)

### 3. Prepare training data with the HMM

The HMM segments the data into the three states: `state1`, `state2`, and
`unknown`. The `"unknown"` state is when the HMM believes that the
probability of either state 1 or 2 is less than 0.90. Additional
`unknown` data is provided by taking the edges of the time series data
(the data before the push-ups begin).

    chop_data_with_hmm <- function(workout_idx,
                                   fit,
                                   wide_data,
                                   full_data,
                                   prob_cutoff = 0.5,
                                   outer_unknown_q = 0.1,
                                   ...) {
      
      modeling_data <- pivot_telemetry_data(full_data, x = smooth_value) 
      
      training_data_1 <- posterior(fit) %.% {
        bind_cols(
          modeling_data %>% filter(idx %in% wide_data$idx)
        )
        mutate(state = case_when(
          S1 > prob_cutoff ~ "state1",
          S2 > prob_cutoff ~ "state2",
          TRUE ~ "IGNORE"
        ))
        filter(state != "IGNORE")
      }

      if (workout_idx %in% DATASETS_TO_REVERSE_STATES) {
        message("Flipping state assignments.")
        training_data_1 <- training_data_1 %.% {
          mutate(state = case_when(
            state == "state1" ~ "state2",
            state == "state2" ~ "state1",
            TRUE ~ state
          ))
        }
      }

      training_data_2 <- modeling_data %.% {
        filter(
          time_step < quantile(time_step, 0.1) |
            time_step > quantile(time_step, 1 - 0.1)
        )
        add_column(state = "unknown")
      }

      bind_rows(training_data_1, training_data_2)
    }

    if (!"full_data" %in% colnames(chopped_pushup_hmms)) {
      chopped_pushup_hmms <- chopped_pushup_hmms %>%
        left_join(
          pushup_data %>% select(workout_idx, full_data = data),
          by = "workout_idx"
        )
    }

    chopped_pushup_hmms$classifier_data <- pmap(chopped_pushup_hmms, chop_data_with_hmm)

    #> Flipping state assignments.
    #> Flipping state assignments.
    #> Flipping state assignments.

Save this collection of data for use in other notebooks.

    saveRDS(
      chopped_pushup_hmms,
      file.path("cache", "hmm_processed_pushup_data.rds")
    )

    chopped_pushup_hmms %.% {
      select(workout_idx, classifier_data)
      unnest(classifier_data)
      count(workout_idx, state)
      pivot_wider(-state, names_from = "state", values_from = "n")
    }

    #> # A tibble: 6 x 4
    #>   workout_idx state1 state2 unknown
    #>         <int>  <int>  <int>   <int>
    #> 1           1    316    355     336
    #> 2           2    365    253     310
    #> 3           3    427    310     370
    #> 4           4    390    316     354
    #> 5           5    477    316     398
    #> 6           6    433    288     362

A t-SNE plot of the data made to train the classifiers.

    d <- chopped_pushup_hmms$classifier_data[[1]]

    training_data_tsne <- d %.% {
      ~ ~original_data <- .
      select(-S1, -S2, -time_step, -state)
      as.data.frame()
      column_to_rownames("idx")
      unique()
      ~ ~min_data <- .
      Rtsne::Rtsne()
      .$Y
      as.data.frame()
      mutate(idx = as.numeric(rownames(min_data)))
      left_join(original_data, by = "idx")
    }

    training_data_tsne %>%
      ggplot(aes(V1, V2, color = state)) +
      geom_jitter(size = 1.4, alpha = 0.6, height = 0.5, width = 0.5) +
      scale_color_manual(values = c("orange", "purple", "black")) +
      labs(
        x = "t-SNE 1",
        y = "t-SNE 2",
        title = "t-SNE of push-up telemetry data"
      )

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### 4. Train a classifier with the HMM-prepared training data.

The main code that interfaces with ‘TidyModels’ is held in
`src/tidymodel-helpers.R`. Below are wrappers for four different
classification model types.

    # KNN classifier.
    run_knn_workflow <- function(data, neighbors = 5) {
      # Model specification.
      knn_spec <- nearest_neighbor(
        mode = "classification",
        neighbors = neighbors
      ) %>%
        set_engine("kknn")
      run_classifier_workflow(data, knn_spec)
    }

    # Random forest classifier.
    run_rf_workflow <- function(data, mtry = 3, trees = 100) {
      rf_spec <- rand_forest(
        mode = "classification",
        mtry = mtry,
        trees = trees,
        min_n = 50
      ) %>%
        set_engine("ranger")
      run_classifier_workflow(data, rf_spec)
    }

    # Naive Bayes classifier.
    run_nb_workflow <- function(data, smoothness = 1, Laplace = 0) {
      nb_spec <- naive_Bayes(
        mode = "classification",
        smoothness = smoothness,
        Laplace = Laplace
      ) %>%
        set_engine("klaR")
      run_classifier_workflow(data, nb_spec)
    }

    # SVM classifier.
    run_svm_workflow <- function(data, cost = 1, rbf_sigma = 1) {
      svm_spec <- svm_rbf(
        mode = "classification",
        cost = cost,
        rbf_sigma = rbf_sigma
      ) %>%
        set_engine("kernlab")
      run_classifier_workflow(data, svm_spec)
    }


    eg_training_data <- chopped_pushup_hmms$classifier_data[[1]]

    # Examples of running each model type.
    bind_rows(
      run_knn_workflow(eg_training_data),
      run_rf_workflow(eg_training_data),
      run_nb_workflow(eg_training_data),
      run_svm_workflow(eg_training_data)
    )

    #> # A tibble: 4 x 27
    #>   fit_model train_pred_prob train_roc_curve train_roc_auc test_pred_prob
    #>   <list>    <list>          <list>                  <dbl> <list>        
    #> 1 <workflo… <tibble [756 ×… <tibble [56 × …         1.00  <tibble [251 …
    #> 2 <workflo… <tibble [756 ×… <tibble [466 ×…         1.00  <tibble [251 …
    #> 3 <workflo… <tibble [756 ×… <tibble [2,268…         0.999 <tibble [251 …
    #> 4 <workflo… <tibble [756 ×… <tibble [2,274…         1.00  <tibble [251 …
    #> # … with 22 more variables: test_roc_curve <list>, test_roc_auc <dbl>,
    #> #   train_pred_class <list>, train_sensitivity <dbl>, train_specificity <dbl>,
    #> #   train_precision <dbl>, train_mcc <dbl>, train_fmeasure <dbl>,
    #> #   train_accuracy <dbl>, train_kap <dbl>, train_ppv <dbl>, train_npv <dbl>,
    #> #   test_pred_class <list>, test_sensitivity <dbl>, test_specificity <dbl>,
    #> #   test_precision <dbl>, test_mcc <dbl>, test_fmeasure <dbl>,
    #> #   test_accuracy <dbl>, test_kap <dbl>, test_ppv <dbl>, test_npv <dbl>

#### KNN hyperparameter tuning

    n_rep <- 10
    coarse_knn_k <- seq(2, 50, 5)
    coarse_knn_k <- rep(coarse_knn_k, each = n_rep)

    stash("coarse_tuning_knn", depends_on = c("eg_training_data", "coarse_knn_k"), {
      coarse_tuning_knn <- map_dfr(
        coarse_knn_k,
        ~ run_knn_workflow(eg_training_data, neighbors = .x)
      ) %>%
        mutate(
          neighbors = coarse_knn_k,
          rep = rep(1:n_rep, n_distinct(coarse_knn_k))
        )
    })

    #> Loading stashed object.

    plot_classifier_tuning_results <- function(df, x, ...) {
      x_breaks <- df %>%
        pull({{ x }}) %>%
        unlist() %>%
        unique()
      other_cols <- rlang::enquos(...)
      df %.%
        {
          select(
            {{ x }}, !!!other_cols, rep,
            train_roc_auc, test_roc_auc,
            train_sensitivity:test_npv
          )
          select(-test_pred_class)
          pivot_longer(-c({{ x }}, !!!other_cols, rep), names_to = "metric")
          mutate(
            test_train = ifelse(str_detect(metric, "^train"), "train", "test"),
            test_train = factor(test_train, levels = c("train", "test")),
            metric = str_remove(metric, "^test_|^train_")
          )
          group_by({{ x }}, !!!other_cols, metric, test_train)
          summarise(
            value_stddev = sd(value),
            value = mean(value)
          )
          ungroup()
          mutate(
            value_plus = value + value_stddev,
            value_minus = value - value_stddev
          )
        } %>%
        ggplot(aes({{ x }}, value, color = metric)) +
        facet_wrap(~test_train, nrow = 2) +
        geom_linerange(aes(ymin = value_minus, ymax = value_plus), alpha = 0.5) +
        geom_line(aes(group = metric), alpha = 0.75) +
        geom_point(alpha = 0.9) +
        scale_x_continuous(breaks = x_breaks)
    }

    plot_classifier_tuning_results(coarse_tuning_knn, x = neighbors)

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

    n_rep <- 20
    fine_knn_k <- seq(15, 31, 1)
    fine_knn_k <- rep(fine_knn_k, each = n_rep)

    stash("fine_tuning_knn", depends_on = c("eg_training_data", "fine_knn_k"), {
      fine_tuning_knn <- map_dfr(
        fine_knn_k,
        ~ run_knn_workflow(eg_training_data, neighbors = .x)
      ) %>%
        mutate(
          neighbors = fine_knn_k,
          rep = rep(1:n_rep, n_distinct(fine_knn_k))
        )
    })

    #> Loading stashed object.

    plot_classifier_tuning_results(fine_tuning_knn, neighbors)

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

#### Random forest hyperparameter tuning

    n_rep <- 10
    coarse_rf_grid <- expand.grid(
      mtry = 1:6,
      trees = seq(10, 100, 10),
      rep = 1:n_rep
    )

    stash("coarse_tuning_rf", depends_on = c("eg_training_data", "coarse_rf_grid"), {
      coarse_tuning_rf <- pmap(coarse_rf_grid, function(mtry, trees, ...) {
        run_rf_workflow(data = eg_training_data, mtry = mtry, trees = trees)
      }) %>%
        bind_rows() %>%
        bind_cols(coarse_rf_grid)
    })

    #> Loading stashed object.

    plot_classifier_tuning_results(coarse_tuning_rf, x = trees, mtry) +
      facet_grid(mtry ~ test_train)

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

    n_rep <- 10
    fine_rf_grid <- expand.grid(
      mtry = 1,
      trees = seq(2, 20, 2),
      rep = 1:n_rep
    )

    stash("fine_tuning_rf", depends_on = c("eg_training_data", "fine_rf_grid"), {
      fine_tuning_rf <- pmap(fine_rf_grid, function(mtry, trees, ...) {
        run_rf_workflow(data = eg_training_data, mtry = mtry, trees = trees)
      }) %>%
        bind_rows() %>%
        bind_cols(fine_rf_grid)
    })

    #> Loading stashed object.

    plot_classifier_tuning_results(fine_tuning_rf, x = trees, mtry)

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

#### Naive Bayes hyperparameter tuning

    n_rep <- 20
    coarse_nb_grid <- expand_grid(
      smoothness = seq(0.5, 3, 0.5),
      rep = 1:n_rep
    )

    stash("coarse_tuning_nb", depends_on = c("eg_training_data", "coarse_nb_grid"), {
      coarse_tuning_nb <- pmap(coarse_nb_grid, function(smoothness, ...) {
        suppressWarnings(
          run_nb_workflow(eg_training_data, smoothness = smoothness, Laplace = 0)
        )
      }) %>%
        bind_rows() %>%
        bind_cols(coarse_nb_grid)
    })

    #> Loading stashed object.

    plot_classifier_tuning_results(coarse_tuning_nb, x = smoothness)

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

#### SVM hyperparameter tuning

    n_rep <- 5
    coarse_svm_grid <- expand_grid(
      cost = seq(5, 20, 6),
      rbf_sigma = 10^(seq(-3, 0, 1)),
      rep = 1:n_rep
    )

    stash("coarse_tuning_svm", depends_on = c("eg_training_data", "coarse_svm_grid"), {
      coarse_tuning_svm <- pmap(coarse_svm_grid, function(cost, rbf_sigma, ...) {
        run_svm_workflow(eg_training_data, cost = cost, rbf_sigma = rbf_sigma)
      }) %>%
        bind_rows() %>%
        bind_cols(coarse_svm_grid)
    })

    #> Loading stashed object.

    plot_classifier_tuning_results(coarse_tuning_svm, x = cost, rbf_sigma) +
      facet_grid(rbf_sigma ~ test_train, scales = "free_y")

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

    n_rep <- 10
    fine_svm_grid <- expand_grid(
      cost = seq(5, 20, 2),
      rbf_sigma = 10^(seq(-3, 0, 1)),
      rep = 1:n_rep
    )

    stash("fine_tuning_svm", depends_on = c("eg_training_data", "fine_svm_grid"), {
      fine_tuning_svm <- pmap(fine_svm_grid, function(cost, rbf_sigma, ...) {
        run_svm_workflow(eg_training_data, cost = cost, rbf_sigma = rbf_sigma)
      }) %>%
        bind_rows() %>%
        bind_cols(fine_svm_grid)
    })

    #> Loading stashed object.

    plot_classifier_tuning_results(fine_tuning_svm, x = cost, rbf_sigma) +
      facet_grid(rbf_sigma ~ test_train)

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

### Table of best classifier hyperparameters

| **Model**     | **Best Parameters**         |
|---------------|-----------------------------|
| kNN           | neighbors: 20               |
| Random Forest | mtry: 1, trees: 16          |
| Naive Bayes   | smoothness: 0.5, Laplace: 0 |
| SVM           | cost: 20, rbf\_sigma: 1     |

I have decided to drop the naive Bayes classifier from further
experimentation as it tends to be a bit tricky to train and tune.

### Deciding on a classification model

    get_fit_model <- function(model_type = c("knn", "rf", "svm"), data) {
      model_type <- str_to_lower(model_type[[1]])
      model_data <-
        if (model_type == "knn") {
          return(run_knn_workflow(eg_training_data, neighbors = 20))
        } else if (model_type == "rf") {
          return(run_rf_workflow(eg_training_data, mtry = 1, trees = 16))
        } else if (model_type == "svm") {
          return(run_svm_workflow(eg_training_data, cost = 20, rbf_sigma = 1))
        } else {
          stop(glue::glue("Unrecognized model type: {model_type}"))
        }
    }


    stash(
      "optimal_models_cross_validation",
      depends_on = "chopped_pushup_hmms",
      {
        optimal_models_cross_validation <- expand_grid(
          train_workout_idx = seq(1, max(chopped_pushup_hmms$workout_idx)),
          model_type = c("knn", "rf", "svm")
        ) %.% {
          left_join(
            chopped_pushup_hmms %>% select(workout_idx, classifier_data),
            by = c("train_workout_idx" = "workout_idx")
          )
          mutate(
            model_res = map2(model_type, classifier_data, get_fit_model),
            fit_model = map(model_res, ~ .x$fit_model[[1]]),
            training_metrics = map(model_res, ~ .x %>% select(-fit_model))
          )
          select(-model_res, -classifier_data)
          add_column(test_workout_idx = list(seq(1, max(chopped_pushup_hmms$workout_idx))))
          unnest(test_workout_idx)
          left_join(
            chopped_pushup_hmms %>% select(workout_idx, classifier_data),
            by = c("test_workout_idx" = "workout_idx")
          )
          group_by(train_workout_idx, model_type, test_workout_idx)
          mutate(
            testing_metrics = map2(fit_model, classifier_data, pushup_classification_metrics),
            testing_roc = map2(fit_model, classifier_data, pushup_classification_roc)
          )
          unnest(testing_metrics)
          unnest(testing_roc)
        }
      }
    )

    #> Updating stash.

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 365
    #> 'state2': 253

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    #> Warning: Problem with `mutate()` input `testing_metrics`.
    #> x While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316
    #> ℹ Input `testing_metrics` is `map2(fit_model, classifier_data, pushup_classification_metrics)`.

    #> Warning: While computing multiclass `precision()`, some levels had no predicted events (i.e. `true_positive + false_positive = 0`). 
    #> Precision is undefined in this case, and those levels will be removed from the averaged result.
    #> Note that the following number of true events actually occured for each problematic event level:
    #> 'state1': 390
    #> 'state2': 316

    optimal_models_cross_validation %.%
      {
        select(
          train_workout_idx, model_type, test_workout_idx,
          sensitivity, specificity, precision, accuracy, roc_auc
        )
        pivot_longer(
          -c(train_workout_idx, model_type, test_workout_idx),
          names_to = "metric",
          values_to = "value"
        )
        filter(train_workout_idx != test_workout_idx)
        group_by(train_workout_idx, model_type, metric)
        summarise(
          avg_value = mean(value),
          sd_value = sd(value)
        )
        ungroup()
        mutate(
          value_up = avg_value + sd_value,
          value_dn = avg_value - sd_value,
          train_workout_idx = glue("training\nwrkt #{train_workout_idx}")
        )
      } %>%
      ggplot(aes(metric, avg_value)) +
      facet_grid(train_workout_idx ~ model_type) +
      geom_line(group = "a") +
      geom_linerange(
        aes(ymin = value_dn, ymax = value_up),
        alpha = 1
      ) +
      geom_point() +
      scale_x_discrete(expand = c(0.07, 0.07)) +
      scale_y_continuous(
        expand = expansion(mult = c(0.02, 0.02)),
      ) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1)
      ) +
      labs(
        x = NULL,
        y = "average value"
      )

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

    optimal_models_cross_validation %.%
      {
        filter(train_workout_idx != test_workout_idx)
        select(
          train_workout_idx, model_type, test_workout_idx,
          sensitivity, specificity, precision, accuracy, roc_auc, mcc
        )
        pivot_longer(
          -c(train_workout_idx, model_type, test_workout_idx),
          names_to = "metric",
          values_to = "value"
        )
      } %>%
      ggplot(aes(x = model_type, y = value)) +
      facet_wrap(~metric, nrow = 2, scales = "free") +
      geom_boxplot(aes(color = model_type), outlier.shape = NA) +
      geom_jitter(aes(color = model_type), height = 0, width = 0.3, alpha = 0.3)

    #> Warning: Removed 10 rows containing non-finite values (stat_boxplot).

    #> Warning: Removed 10 rows containing missing values (geom_point).

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

    optimal_models_cross_validation %.%
      {
        select(
          train_workout_idx, model_type, test_workout_idx,
          roc_curve
        )
        unnest(roc_curve)
        mutate(
          line_group = paste(train_workout_idx, test_workout_idx),
          train_is_test = train_workout_idx == test_workout_idx
        )
      } %>%
      ggplot(aes(1 - specificity, sensitivity)) +
      facet_grid(.level ~ model_type) +
      geom_line(
        aes(
          group = line_group,
          color = factor(test_workout_idx),
          lty = train_is_test
        ),
        alpha = 0.5
      ) +
      scale_color_brewer(
        type = "qual",
        palette = "Dark2",
        guide = FALSE
      ) +
      scale_linetype_manual(
        values = c("TRUE" = 2, "FALSE" = 1),
        guide = FALSE
      ) +
      labs(
        x = "1 - specificity",
        y = "sensitivity",
        title = "ROC of different model types",
        subtitle = "The colors indicate different workout data sets and the dashed lines are when\nthe model was trained on that workout data set."
      )

![](05_008_hmm_pipelines_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

### 5. Applying the trained model to new data

This step will require a more refined pipeline than I have developed in
this notebook. I have skipped it here, but it will be in the next one.

------------------------------------------------------------------------

Conclusions
-----------

### Data processing

I think smoothing and scaling the data is required for the HMM and
likely helps the classifiers. I need to better account for how new data
will be collected and classified in real time. For example, the training
data’s mean and standard deviation will need to be stored for scaling
the new data.

### HMM

I need to remember that the states that the HMM identifies need to be
mapped to the states of a push-up. The chopping by the HMM and addition
of “unknown” data worked quite well.

### Classifier models

I would recommend moving forward with the random forest. It outperformed
the kNN and SVM classifiers on most metrics and is the most consistent
across data sets and on new, unseen data.
