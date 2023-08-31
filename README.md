# Network Learning and Sparse Estimation 

Repository for all code related to the seminar. This work focuses on the neighborhood selection
approach by [Meinshausen and Bühlmann (2006)](https://arxiv.org/abs/math/0608017) in comparison to GLASSO by [Friedman et al. (2008)](https://arxiv.org/abs/0708.3517).

Using the `huge` package, the methods are applied to different graph-topologies and compared in the seminar paper.


# Overview
```
├── .gitignore                  <- Blacklisting files for version control.
├── analysis.R                  <- Script including the processing and analysis of the generated data.
├── ebic_adjusted.R             <- Script including the function for the adjusted eBIC computation to avoid selecting a model without variables.
├── exp_script.R                <- R-script for running an isolated experiment on a range of different dimensions.
├── figures.R                   <- Script for generating the figures from the paper.
├── utils.R                     <- Script entailing different helper functions to compute the metrics.
├── LICENSE                     <- License file (MIT).
├── main.py                     <- Main file for running the experiment.
├── MB_DGF.R                    <- Includes a function that implements the data-generating process from Meinshausen and Bühlmann (2006).
├── README.md                   <- This file.
├── requirements.R              <- required packages.
└── tests.R                     <- Tests for `MB_DGF.R`.
```