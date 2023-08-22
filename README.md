# Network Learning and Sparse Estimation 

Repository for all code related to the seminar. This work focuses on the neighborhood selection
approach by [Meinshausen and Bühlmann (2006)](https://arxiv.org/abs/math/0608017) in comparison to GLASSO by [Friedman et al. (2008)](https://arxiv.org/abs/0708.3517).

Using the `huge` package, the methods are applied to different graph-topologies and compared in the seminar paper.


# Overview
```
├── figures.R                   <- Script for generating the figures from the paper.
├── utils.R                     <- Script entailing different helper functions to compute the metrics.
├── MB_DGF.R                    <- Includes a function that implements the data-generating process from Meinshausen and Bühlmann (2006).
├── main.R                      <- Main file for running the predictions and evaluations.
├── tests.R                     <- Tests for `MB_DGF.R`.
├── LICENSE                     <- License file (MIT).
├── .gitignore                  <- Blacklisting files for version control.
└── README.md                   <- This file.
```