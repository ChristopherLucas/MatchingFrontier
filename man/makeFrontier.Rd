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
makeFrontier(dataset, treatment, outcome, match.on, QOI = "FSATT",
             metric = "Mahal", ratio = "variable", breaks = NULL)
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

# set seed for sampling a small data set for this example
set.seed(02139) 
lalonde <- lalonde[c(which(lalonde$treat == 1),
                     sample(which(lalonde$treat == 0), 500)), ]

match.on <- colnames(lalonde)[!(colnames(lalonde) \%in\% c('re78', 'treat'))]
my.frontier <- makeFrontier(dataset = lalonde,
                            treatment = 'treat',
                            outcome = 're78',
                            match.on = match.on)
}