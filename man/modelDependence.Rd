\name{modelDependence}
\alias{modelDependence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Compute the Athey-Imbens measure of sensitivity to model
  specification.
}
\description{
  \code{modelDependence()} is used to compute the Athey-Imbens
  measure of sensitivity to model specification.
}
\usage{
modelDependence(dataset, treatment, base.form, verbose = TRUE,
seed = 1, cutpoints = NA, median = TRUE)
}
\arguments{
  \item{dataset}{
    A data frame containing the variables in the model.
  }
  \item{treatment}{
    The treatment (quantity of interest). The measure of
    model dependence is with respect to estimates of this
    quantity. Must be in base.form.
  }
  \item{base.form}{
    The base formula that is to be evaluated.
  }
  \item{verbose}{
    If TRUE, additional information is printed.
  }
  \item{seed}{
    Seed for the random number generator.
  }
  \item{cutpoints}{
    A list where the keys are variables names and the
    values are cutpoints. If specified, cutpoints for
    these variables will not be estimated. Otherwise,
    cutpoints are estimated with segmented regression.
  }
  \item{median}{
    If TRUE, the cutpoint is set at the median. If false,
    the cutpoint is estimated with segmeted (piecewise)
    regression.
  }
}
\examples{
data(lalonde)

treatment <- 'treat'
base.form <- as.formula('re78 ~ treat + age + education
                         + black + hispanic + married +
                         nodegree + re74 + re75')

md <- modelDependence(lalonde, treatment, base.form,
                      cutpoints = list('age' = mean(lalonde$age)))
print(md)
}
\references{
  Athey, Susan, and Guido W. Imbens. "A Measure of Robustness to Misspecification." (2014).
}
