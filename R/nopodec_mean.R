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
#' \item \code{common_support} logical indicating if in or out the common support;
#' \item \code{ybar} average of the y variable, weighted by the given weights;
#' \item \code{ybar_C_A} counterfactual average y of group A as if they had the same distribution of characteristics of group B. This is computed in the common support only and for group A individuals. It is computed with the weights \code{w_AB} that result from \code{\link{reweight_strata_all2}};
#' \item \code{ybar_C_B} counterfactual average y of group B as if they had the same distribution of characteristics of group A. This is computed in the common support only and for group B individuals. It is computed with the weights \code{w_BA} that result from \code{\link{reweight_strata_all2}};
#' \item \code{Nhat} estimate of the number of individuals.
#' }
#' The number of rows is given by the combinations of the distinct values of
#' the first two columns: \code{treatment} and \code{common_support}.
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
  treatment <- attributes(.reweight_strata_all)[["treatment"]]
  if(is.null(weights)) weights <- attributes(.reweight_strata_all)[["weights"]]
  if(is.null(y)) y <- attributes(.reweight_strata_all)[["y"]]

  # Preparazioni per summarise_
  # ybar_marginals <- lazyeval::interp(~stats::weighted.mean(x, w),
  #                                    x = as.name(y), w = as.name(weights))
  # ybar_counterfactual_A <- lazyeval::interp(~stats::weighted.mean(x, w_AB),
  #                                           x = as.name(y))
  # ybar_counterfactual_B <- lazyeval::interp(~stats::weighted.mean(x, w_BA),
  #                                           x = as.name(y))
  # nhat <- lazyeval::interp(~sum(w), w = as.name(weights))

  y_sim <- rlang::sym(y); w_sim <- rlang::sym(weights)
  w_AB <- rlang::sym("w_AB"); w_BA <- rlang::sym("w_BA")
  y_sim <- rlang::enquo(y_sim); w_sim <- rlang::enquo(w_sim)
  w_AB <- rlang::enquo(w_AB); w_BA <- rlang::enquo(w_BA)
  ybar <- "ybar"; ybar_C_A <- "ybar_C_A"; ybar_C_B <- "ybar_C_B"; Nhat <- "Nhat"


  # Medie partitions
  medie_partitions <- .reweight_strata_all %>%
    gby_(c(treatment, "common_support")) %>%
    # summarise2_(.dots = stats::setNames(
    #   list(ybar_marginals, ybar_counterfactual_A, ybar_counterfactual_B, nhat),
    #   c("ybar", "ybar_C_A", "ybar_C_B", "Nhat")))
    dplyr::summarise(
      !! ybar := stats::weighted.mean(!! y_sim, !! w_sim),
      !! ybar_C_A := stats::weighted.mean(!! y_sim, !! w_AB),
      !! ybar_C_B := stats::weighted.mean(!! y_sim, !! w_BA),
      !! Nhat := sum(!! w_sim)
    )

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
#' \item \code{ybar}: average of the y variable;
#' \item \code{Nhat}: estimate of the number of individuals.
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
  # ybar_marginals <- lazyeval::interp(~stats::weighted.mean(x, w),
  #                                    x = as.name(y), w = as.name(weights))
  # nhat <- lazyeval::interp(~sum(w), w = as.name(weights))

  y_sim <- rlang::sym(y); w_sim <- rlang::sym(weights)
  y_sim <- rlang::enquo(y_sim); w_sim <- rlang::enquo(w_sim)
  ybar <- "ybar"; Nhat <- "Nhat"

  # Medie marginali
  medie_marginali <- .reweight_strata_all %>%
    gby_(treatment) %>%
    # summarise2_(.dots = stats::setNames(
    #   list(ybar_marginals, nhat),
    #   c("ybar", "Nhat")))
    dplyr::summarise(
      !! ybar := stats::weighted.mean(!! y_sim, !! w_sim),
      !! Nhat := sum(!! w_sim)
    )

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




