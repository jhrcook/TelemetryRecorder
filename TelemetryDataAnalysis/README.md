# Analysis of Telemetry Data

1. [Exploration of the data and modeling the data](analysis/05_005_hidden-markov-modeling.md)
2. [Experimentation of a data processing and classification workflow](analysis/05_008_hmm_pipelines.md)
3. [Hyperparameter tuning of the Random Forest Classifier](analysis/05_010_random-forest-hyperparam-tuning.md)
3. [Refinement of the classifier training and implementation pipeline](analysis/05_011_classifier-training-implementation-pipeline.md) (in progress)

## Classifier training and implementation pipepline

See the [original notebook](analysis/05_011_classifier-training-implementation-pipeline.md) for details.

Below is a flowchart of the pipeline used for this modeling process.

<img src="analysis/05_011_classifier-training-implementation-pipeline_files/pipeline-diagram.png" width="600px">

A random forest fit on one set of data was able to reliably identify the states of the exercise from completely new push-up data.

![](analysis/05_011_classifier-training-implementation-pipeline_files/figure-gfm/unnamed-chunk-17-1.png)

---

### To-Do

1. (DONE) Update [Experimentation of a data processing and classification workflow](analysis/05_008_hmm_pipelines.md) to use the smoothed (not scaled) data for training the RF.
2. Revisit [Hyperparameter tuning of the Random Forest Classifier](analysis/05_010_random-forest-hyperparam-tuning.md) with the updated input data and better understanding of how model sees the data.
  - use a lower `max.depth`
3. In a new notebook, experiment with using another "meta"-classifier to distinguish between unknown and push-up states. 
  - SVM seems promising
  - keep in mind how this process would work with real streams of data
    - some sort of animation could be useful (using the t-SNE or PCA projections)
