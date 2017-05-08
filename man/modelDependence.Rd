\name{modelDependence}
\alias{modelDependence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Compute the Athey-Imbens measure of sensitivity to model
  specification.
}
\description{
  \code{modelDependence()} is used to compute the Athey-Imbens
  measure of sensitivity to model specification or to estimate
  the range of possible treatment effect estimates by extreme
  bounds. If a base.form is given, the Athey-Imbens estimates
  are calculated. To use extreme bounds, give either
  model.dependence.ests or specifictions.
}
\usage{
modelDependence(dataset,
         treatment,
         outcome = NULL,
         covariates = NULL,
         model.dependence.ests = NULL,
         verbose = TRUE,
         ratio = 'fixed',
         specifications = NULL,
         base.form = NULL,
         cutpoints = NULL,
         median = TRUE,
         seed = 02138)
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
  \item{outcome}{
    The outcome variable.
  }
  \item{covariates}{
    A vector of the names of covariates necessary to perform
    the extreme bounds (treatment, outcome, and control
    variables).
  }
  \item{model.dependence.ests}{
    For extreme bounds, the number of specifications
    to estimate.
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
  \item{ratio}{
    Either "fixed" or "variable". If fixed, covariates
    are not reweighted according to their matches during
    the extreme bounds procedure. If variable, they are
    reweighted.
  }
  \item{specifications}{
    A vector of model formulas to be used in the
    extreme bounds procedure. If not supplied, they are
    drawn from all possible specifications using up to
    third degree polynomials and double interactions.
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
