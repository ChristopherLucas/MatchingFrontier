\name{NEWS}
\title{NEWS file for the stm package}

\section{Version 0.8.28}{
  \itemize{
    \item Created vignette. 
    \item Fixed bug in makeFrontier(). Previous Mahalanobis
    frontiers are off by a small factor, mistakenly appearing
    nonmonotonic on occasion.
    \item Fixed bug in estimateEffects(). Previous estimates
    on Mahalanobis frontiers use the wrong weights. 
    \item Help files in generateDataset(), plotEstimates(), and
    plotFrontier() improved.
    \item Added model dependence intervals to plotEstimates()
    plots.
    \item Added new function plotMeans() to plot means of
    covariates along the frontier.
    \item Added option to include covariates not matched
    on in the data output by makeFrontier().
    \item Added option to set.seed() for model dependence
    intervals. 
  }
}
\section{Initial CRAN release version 0.3.17}{
  \itemize{
    \item First version released on CRAN.
    \item This is a beta release with many changes to come. 
}}

