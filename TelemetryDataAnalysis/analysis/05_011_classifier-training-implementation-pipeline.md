Classifier training and implementation pipeline
================

Goal
----

The goal of this notebook is to construct and refine a pipeline for
training and implementing a classifier that detects the states of a
push-up.

Pipeline
--------

The pipeline was first explored in the notebook [*Hidden Markov
Model-based learning pipelines*](05_008_hmm_pipelines.md). The following
pipeline is based on this previous work, though refined to better suit a
real use-case

### Steps

1.  Chop the raw data within an IQR of the time steps to get just the
    clean push-up data.
2.  Use an HMM to identify the 2 states of the push-up.
3.  Use the HMM to cut the chopped data into the 3 states of a push-up
    (one state as `unknown`).
4.  Train a classifier on this training data.
5.  Apply the classifier to new data as it is being collected.

### API

The key advancement of this notebook in this project needs to be the
construction of a pipeline that can operate under real-world
circumstances. Thus, the API is important because, though the model will
not be implemented in R, it will define the modularity of the pipeline.

`# TODO: use a diagramming tool to outline process`