% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reweight_strata_all.R
\name{reweight_strata_all4}
\alias{reweight_strata_all4}
\title{Estimates adjustment (counterfactual) weights in the common support and joins with the data.}
\usage{
reweight_strata_all4(data, treatment, variables, y, weights = NULL)
}
\arguments{
\item{data}{data frame}

\item{treatment}{column name of the binary variable}

\item{variables}{character vector of the variables' names, for which the common support has to be established}

\item{y}{name of the outcome variable for which you want to make the decomposition}

\item{weights}{name of the weight variable (sample weights). If \code{NULL}
(default value) it uses equal weights for all observations, adding
a column of ones}
}
\value{
An object of class \code{reweighted}. This is a list with the following elements:
  \item{.fhat_strata}{empirical joint frequencies of strata, in and out of the common support. This is the output of \code{\link{fhat_strata2}}}
  \item{.reweight_strata}{adjustment factors to estimate counterfactuals (in the common support). This is the output of \code{\link{reweight_strata2}}}
  \item{.reweight_strata_all}{data joined with the estimated adjustment (counterfactual) weights in the common support. This is the output of \code{\link{reweight_strata_all2}}}
}
\description{
This function estimates adjustment (counterfactual) weights in the common support and joins with the data. For observations out of the common support, the weights are equal to the sample weights (or one, in case of absence of sample weights). Note that this function does the same things of \code{\link{reweight_strata_all2}}, but stores two intermediate results: the output of functions \code{\link{fhat_strata2}} and \code{\link{reweight_strata2}} (see the description in the value section).
}
\examples{
data(invented_wages)
r00 <- reweight_strata_all4(invented_wages, treatment = "gender",
                       variables = c("sector", "education"),
                       y = "wage", weights = "sample_weights")

str(r00)
names(r00)
class(r00)
}
\seealso{
\code{\link{fhat_strata2}}, \code{\link{reweight_strata2}}
        and \code{\link{reweight_strata_all2}}, that are the 3 components of the list
        resulting from this function (an object of class \code{reweighted}).
        For the next steps, see for example \code{\link{nopodec_mean}}, \code{\link{dec_median}}
         and \code{\link{dec_quantile}}.
         See also \code{\link{margin_mean}} and \code{\link{margin_quantile}}, that use the output
         of this function as their input.
}
