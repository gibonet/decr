% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/various_dec.R
\name{dec_quantiles}
\alias{dec_quantiles}
\alias{dec_quantiles.default}
\alias{dec_quantiles.reweighted}
\title{Estimates quantiles of the y variable for the two groups, in and out the common support.
In the common support, counterfactual quantiles of y are estimated.
It also estimates the number of individuals of the two groups.
Moreover, marginal quantiles of the two groups are also computed.}
\usage{
dec_quantiles(...)

\method{dec_quantiles}{default}(
  .reweight_strata_all,
  y = NULL,
  weights = NULL,
  probs = c(0.25, 0.5, 0.75),
  ...
)

\method{dec_quantiles}{reweighted}(.reweighted, ...)
}
\arguments{
\item{...}{arguments passed to or from other methods.}

\item{.reweight_strata_all}{output of \code{\link{reweight_strata_all2}}}

\item{y}{name of the outcome variable for which you want to make the decomposition. If NULL (default), the value is inherited from the attributes of \code{.reweight_strata_all}}

\item{weights}{name of the weight variable (sample weights). If NULL (default), the value is inherited from the attributes of \code{.reweight_strata_all}.}

\item{probs}{numeric vector with the desired quantile levels (all the values should be between 0 and 1). Default value: \code{c(0.25, 0.5, 0.75)}}

\item{.reweighted}{an object of class \code{reweighted} (the output of \code{\link{reweight_strata_all4}})}
}
\value{
A data frame with four, five or six rows, with the following columns:
\itemize{
\item the name of the treatment column used in \code{\link{reweight_strata_all2}};
\item \code{common_support} logical indicating if in or out the common support;
\item \code{yhat} quantiles of the y variable, weighted by the given weights. This is a list where each element is a numeric vector with the estimates of the quantiles at all the levels given by the argument \code{probs};
\item \code{yhat_C_A} counterfactual quantiles y of group A as if they had the same distribution of characteristics of group B. This is computed in the common support only and for group A individuals. It is computed with the weights \code{w_AB} that result from \code{\link{reweight_strata_all2}}. This is a list where each element is a numeric vector with the estimates of the quantiles at all the levels given by the argument \code{probs};
\item \code{yhat_C_B} counterfactual quantiles y of group B as if they had the same distribution of characteristics of group A. This is computed in the common support only and for group B individuals. It is computed with the weights \code{w_BA} that result from \code{\link{reweight_strata_all2}}. This is a list where each element is a numeric vector with the estimates of the quantiles at all the levels given by the argument \code{probs};
\item \code{Nhat} estimate of the number of individuals. This is a list where each element is a numeric vector of length one.
}
The number of rows is given by the combinations of the distinct values of
the first two columns: \code{treatment} and \code{common_support}.
In addition to these rows, there are two more rows at the end, with the
marginal quantiles of the two groups.
In the "typical" case, the resulting data frame will have 4 rows. It can have three rows if all the individuals of one group are in the common support.
In case of no common support or no out-of-support, the data frame will have two rows.
}
\description{
The results in the common support are all the components necessary
to perform a decomposition of the quantile wage difference between
two groups, in two components: one that can be explained by the
difference in the distributions of characteristics between the two groups
(delta_X), and one that cannot be explained by the different
characteristics of the two groups (delta_S).
}
\details{
Note that this function estimates quantiles at different levels
simultaneously, while \code{\link{dec_quantile}} does only one quantile
level.
}
\examples{
data(invented_wages)

# Common support and computation of counterfactual weights
r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
                       variables = c("sector", "education"),
                       y = "wage", weights = "sample_weights")

# Computation of the elements necessary to the decomposition
dec_quantiles(r00)
dec_quantiles(r00, probs = seq(0.05, 0.95, by = 0.05))

data(invented_wages)
r00 <- reweight_strata_all4(invented_wages, treatment = "gender",
                       variables = c("sector", "education"),
                       y = "wage", weights = "sample_weights")

str(r00)
names(r00)
class(r00)

dec_quantiles(r00)

}
