
#### ---- Data set manipulations ---- ####




#### ---- TidyModel interface ---- ####

# Append a prefix to each column name of a data frame.
prefix_colnames <- function(d, prefix) {
  colnames(d) <- paste0(prefix, colnames(d))
  return(d)
}


# Collect classification ROC results.
pushup_classification_roc <- function(fit, data) {
  pred_data <- predict(fit, data, type = "prob") %>%
    bind_cols(data)
  
  roc_curve <- roc_curve(
    pred_data,
    truth = factor(state),
    .pred_state1, .pred_state2, .pred_unknown
  )
  
  roc_auc_est <- roc_auc(
    pred_data,
    truth = factor(state),
    .pred_state1, .pred_state2, .pred_unknown
  )
  
  tibble(
    pred_prob = list(pred_data),
    roc_curve = list(roc_curve),
    roc_auc = roc_auc_est$.estimate[[1]]
  )
}


# Collect classification metrics.
pushup_classification_metrics <- function(fit, data) {
  metric_f <- function(pred_data, fxn) {
    fxn(pred_data, state, .pred_class)$.estimate[[1]]
  }
  
  pred_data <- predict(fit, data, type = "class") %>%
    bind_cols(data) %>%
    mutate(state = factor(state))
  
  tibble(
    pred_class = list(pred_data),
    sensitivity = metric_f(pred_data, sensitivity),
    specificity = metric_f(pred_data, specificity),
    precision = metric_f(pred_data, precision),
    mcc = metric_f(pred_data, mcc),
    fmeasure = metric_f(pred_data, f_meas),
    accuracy = metric_f(pred_data, accuracy),
    kap = metric_f(pred_data, kap),
    ppv = metric_f(pred_data, ppv),
    npv = metric_f(pred_data, npv)
  )
}


classifier_assessment_workflow <- function(fit, train_data, test_data) {
  roc_results_train <- pushup_classification_roc(fit, train_data) %>% 
    prefix_colnames("train_")
  roc_results_test <- pushup_classification_roc(fit, test_data) %>% 
    prefix_colnames("test_")
  
  class_results_train <- pushup_classification_metrics(fit, train_data) %>%
    prefix_colnames("train_")
  class_results_test <- pushup_classification_metrics(fit, test_data) %>%
    prefix_colnames("test_")
  
  bind_cols(
    roc_results_train, roc_results_test, 
    class_results_train, class_results_test
  )
}


run_classifier_workflow <- function(data, model_spec, prop = 0.75) {
  
  # Model recipe.
  model_recipe <- recipe(
    state ~ x + y + z + pitch + roll + yaw,
    data = data
  )
  
  # TidyModels workflow.
  classifer_workflow <- workflow() %>%
    add_model(model_spec) %>%
    add_recipe(model_recipe)
  
  # Split data into training and testing.
  data_split <- initial_split(data, prop = prop, strata = "state")
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Fit the model.
  fit_model <- parsnip::fit(classifer_workflow, data = train_data)
  
  # Get model assessment values.
  model_assessment <- classifier_assessment_workflow(
    fit_model,
    train_data,
    test_data
  )
  
  bind_cols(
    tibble(fit_model = list(fit_model)),
    model_assessment
  )
}
