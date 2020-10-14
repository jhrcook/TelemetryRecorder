# Analysis of Telemetry Data

1. [Exploration of the data and modeling the data](analysis/05_005_hidden-markov-modeling.md)
2. [Experimentation of a data processing and classification workflow](analysis/05_008_hmm_pipelines.md)
3. [Hyperparameter tuning of the Random Forest Classifier](05_010_random-forest-hyperparam-tuning.md)
3. [Refinement of the classifier training and implementation pipeline](analysis/05_011_classifier-training-implementation-pipeline.md) (in progress)

## Classifier training and implementation pipepline

See the [original notebook](analysis/05_011_classifier-training-implementation-pipeline.md) for details.

Below is a flowchart of the pipeline used for this modeling process.

<img src="analysis/05_011_classifier-training-implementation-pipeline_files/pipeline-diagram.png" width="600px">

A random forest fit on one set of data was able to reliably identify the states of the exercise from completely new push-up data.

![](analysis/05_011_classifier-training-implementation-pipeline_files/figure-gfm/unnamed-chunk-17-1.png)

---

### To-Do

1. In a new notebook, focus on tuning all of the random forest hyperparameters available through 'ranger'. Use the information learned for the random forest in [Refinement of the classifier training and implementation pipeline](analysis/05_011_classifier-training-implementation-pipeline.md).
