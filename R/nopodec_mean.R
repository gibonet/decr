#' Estimates y averages for the two groups, in and out the common support.
#' In the common support, counterfactual average y are estimated.
#' It also estimates the number of individuals of the two groups.
#'
#' The results are all the components necessary to perform a
#' decomposition of the average wage difference between two groups,
#' in four components (as in Nopo (2004)).
#'
#' @param ... arguments passed to or from other methods.
#'
#'
#'
#' @export
nopodec_mean <- function(...){
  UseMethod("nopodec_mean")
}


#' @param .reweight_strata_all output of \code{\link{reweight_strata_all2}}
#' @param y name of the outcome variable for which you want to make the decomposition. If NULL (default), the value is inherited from the attributes of \code{.reweight_strata_all}
#' @param weights name of the weight variable (sample weights). If NULL (default), the value is inherited from the attributes of \code{.reweight_strata_all}.
#'
#' @return A data frame with two, three or four rows, with the following columns:
#' \itemize{
#' \item the name of the treatment column used in \code{\link{reweight_strata_all2}};
#' \item common_support logical indicating if in or out the common support;
#' \item ybar average of the y variable, weighted by the given weights;
#' \item ybar_C_A counterfactual average y of group A as if they had the same distribution of characteristics of group B. This is computed in the common support only and for group A individuals. It is computed with the weights \code{w_AB} that result from \code{\link{reweight_strata_all2}};
#' \item ybar_C_B counterfactual average y of group B as if they had the same distribution of characteristics of group A. This is computed in the common support only and for group B individuals. It is computed with the weights \code{w_BA} that result from \code{\link{reweight_strata_all2}};
#' \item Nhat estimate of the number of individuals.
#' }
#' The number of rows is given by the combinations of the distinct values of
#' the first two columns: treatment and common_support.
#' In the "typical" case, the resulting data frame will have 4 rows. It can have three rows if all the individuals of one group are in the common support.
#' In case of no common support or no out-of-support, the data frame will have two rows.
#'
#' @examples
#' data(invented_wages)
#' r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' nopodec_mean(r00)
#'
#' @rdname nopodec_mean
#' @export
nopodec_mean.default <- function(.reweight_strata_all, y = NULL, weights = NULL, ...){
  treatment <- colnames(.reweight_strata_all)[1]
  if(is.null(weights)) weights <- attributes(.reweight_strata_all)[["weights"]]
  if(is.null(y)) y <- attributes(.reweight_strata_all)[["y"]]

  # Preparazioni per summarise_
  ybar_marginals <- lazyeval::interp(~stats::weighted.mean(x, w),
                                     x = as.name(y), w = as.name(weights))
  ybar_counterfactual_A <- lazyeval::interp(~stats::weighted.mean(x, w_AB),
                                            x = as.name(y))
  ybar_counterfactual_B <- lazyeval::interp(~stats::weighted.mean(x, w_BA),
                                            x = as.name(y))
  nhat <- lazyeval::interp(~sum(w), w = as.name(weights))


  # Medie partitions
  medie_partitions <- .reweight_strata_all %>%
    dplyr::group_by_(.dots = c(treatment, "common_support")) %>%
    dplyr::summarise_(.dots = stats::setNames(
      list(ybar_marginals, ybar_counterfactual_A, ybar_counterfactual_B, nhat),
      c("ybar", "ybar_C_A", "ybar_C_B", "Nhat")))

  medie_partitions %>% dplyr::ungroup()
}


# nopodec_mean <- function(.reweight_strata_all, y = NULL, weights = NULL, .reweighted){


#' @param .reweighted an object of class \code{reweighted} (the output of \code{\link{reweight_strata_all4}})
#'
#' @examples
#' data(invented_wages)
#' r00 <- reweight_strata_all4(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' str(r00)
#' names(r00)
#' class(r00)
#'
#' nopodec_mean(r00)
#'
#' @export
#' @rdname nopodec_mean
nopodec_mean.reweighted <- function(.reweighted, ...){
  .reweight_strata_all <- .reweighted[[".reweight_strata_all"]]
  nopodec_mean.default(.reweight_strata_all)
}



#' Estimates average y for groups A and B (marginals) and the number of observations.
#'
#' @param ... arguments passed to or from other methods.
#' @export
margin_mean <- function(...){
  UseMethod("margin_mean")
}


#' @inheritParams nopodec_mean.default
#'
#' @return a data frame with two rows (one for each group) and the follwing three columns:
#' \itemize{
#' \item the name of the treatment column used in \code{\link{reweight_strata_all2}};
#' \item ybar: average of the y variable;
#' \item Nhat: estimate of the number of individuals.
#' }
#'
#' @examples
#' data(invented_wages)
#'
#' r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
#'                             variables = c("sector", "education"),
#'                             y = "wage", weights = "sample_weights")
#'
#' margin_mean(r00)
#'
#' @rdname margin_mean
#' @export
margin_mean.default <- function(.reweight_strata_all, y = NULL, weights = NULL, ...){
  treatment <- colnames(.reweight_strata_all)[1]
  if(is.null(weights)) weights <- attributes(.reweight_strata_all)[["weights"]]
  if(is.null(y)) y <- attributes(.reweight_strata_all)[["y"]]

  # Preparazioni per summarise_
  ybar_marginals <- lazyeval::interp(~stats::weighted.mean(x, w),
                                     x = as.name(y), w = as.name(weights))
  nhat <- lazyeval::interp(~sum(w), w = as.name(weights))

  # Medie marginali
  medie_marginali <- .reweight_strata_all %>%
    dplyr::group_by_(.dots = treatment) %>%
    dplyr::summarise_(.dots = stats::setNames(
      list(ybar_marginals, nhat),
      c("ybar", "Nhat")))

  medie_marginali %>% dplyr::ungroup()
}



#' @param .reweighted an object of class \code{reweighted} (the output of \code{\link{reweight_strata_all4}})
#'
#' @examples
#' data(invented_wages)
#' r00 <- reweight_strata_all4(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' str(r00)
#' names(r00)
#' class(r00)
#'
#' margin_mean(r00)
#'
#' @export
#' @rdname margin_mean
margin_mean.reweighted <- function(.reweighted, ...){
  .reweight_strata_all <- .reweighted[[".reweight_strata_all"]]
  margin_mean.default(.reweight_strata_all)
}




