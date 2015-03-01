\name{makeFrontier}
\alias{makeFrontier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Compute the balance - sample size frontier.
}
\description{
  \code{makeFrontier()} computes the balance - frontier sample
    size and can be used with \code{estimateEffects} to estimate
    effects along the balance - sample size frontier.
}
\usage{
makeFrontier(dataset, treatment, outcome, match.on, 
             keep.vars = NULL, QOI = 'FSATT', metric = 'Mahal',
             ratio = 'fixed', breaks = NULL)
}
\arguments{
  \item{dataset}{
    The data set contain containing the treatment, outcome, and variable
    to match on.
  }
  \item{treatment}{
    The name of the treatment.
  }
  \item{outcome}{
    The name of the outcome.
  }
  \item{match.on}{
    A vector of colnames indicating which variables are to be matched on.
  }
  \item{keep.vars}{
    A character vector of variable names that are not in treatment,
    outcome, or 'match.on' but that the user would like to store
    in the data, either for calculation of model depedence
    intervals or for use in exported data sets. 
  }
  \item{QOI}{
    The quantity of interest to be estimated. By default, feasible
    sample average treatment effect on the treated or FSATT. The other
    option is SATT (sample average treatment effect on the treated).
  }
  \item{metric}{
    The metric used to measure imbalance. Defaults to average
    mahalanobis distance to nearest match. The other option is L1.
  }
  \item{ratio}{
    Variable or fixed ratio. See King, Lucas, and Nielsen for details.
  }
  \item{breaks}{
    Can be used with L1 to provide user-specified breaks.
  }
}
\examples{
data(lalonde)

match.on <- colnames(lalonde)[!(colnames(lalonde) \%in\% c('re78', 'treat'))]
my.frontier <- makeFrontier(dataset = lalonde,
                            treatment = 'treat',
                            outcome = 're78',
                            match.on = match.on)
}
\references{
  King, Gary, Christopher Lucas, and Richard
  Nielsen. "The Balance-Sample 
  Size Frontier in Matching Methods for Causal Inference." (2015).
}
