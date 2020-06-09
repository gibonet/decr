
#' Empirical cumulative distribution function (ecdf) at value y for the two groups, in and out the common support.
#' In the common support, counterfactual ecdf are estimated.
#' It also estimates the number of individuals of the two groups.
#'
#' The results are all the components necessary to perform a
#' decomposition of the ecdf difference between two groups,
#' in four components (as in Nopo (2004)).
#'
#' @param ... arguments passed to or from other methods.
#'
#'
#'
#' @export
nopodec_Fhat <- function(...){
  UseMethod("nopodec_Fhat")
}



#' @inheritParams nopodec_mean.default
#' @param value value of y for which the ecdf is estimated
#'
#' @examples
#' data(invented_wages)
#' r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' nopodec_Fhat(r00, value = 5000)
#'
#' @rdname nopodec_Fhat
#' @export
nopodec_Fhat.default <- function(.reweight_strata_all, y = NULL, weights = NULL, value, ...){
  treatment <- colnames(.reweight_strata_all)[1]
  if(is.null(weights)) weights <- attributes(.reweight_strata_all)[["weights"]]
  if(is.null(y)) y <- attributes(.reweight_strata_all)[["y"]]

  # Preparazioni per summarise_
  # Fhat_marginals <- lazyeval::interp(~sum((x <= value) * w) / sum(w),
  #                                    x = as.name(y), w = as.name(weights))
  # Fhat_counterfactual_A <- lazyeval::interp(~sum((x <= value) * w_AB) / sum(w_AB),
  #                                           x = as.name(y))
  # Fhat_counterfactual_B <- lazyeval::interp(~sum((x <= value) * w_BA) / sum(w_BA),
  #                                           x = as.name(y))
  # nhat <- lazyeval::interp(~sum(w), w = as.name(weights))
  y_sim <- rlang::sym(y); w_sim <- rlang::sym(weights)
  w_AB <- rlang::sym("w_AB"); w_BA <- rlang::sym("w_BA")
  y_sim <- rlang::enquo(y_sim); w_sim <- rlang::enquo(w_sim)
  w_AB <- rlang::enquo(w_AB); w_BA <- rlang::enquo(w_BA)

  Fhat <- "Fhat"; Fhat_C_A <- "Fhat_C_A"; Fhat_C_B <- "Fhat_C_B"; Nhat <- "Nhat"

  # Ecdf partitions
  Fhat_partitions <- .reweight_strata_all %>%
    gby_(c(treatment, "common_support")) %>%
    # summarise2_(.dots = stats::setNames(
    #   list(Fhat_marginals, Fhat_counterfactual_A, Fhat_counterfactual_B, nhat),
    #   c("Fhat", "Fhat_C_A", "Fhat_C_B", "Nhat")))
    dplyr::summarise(
      !! Fhat := sum((!! y_sim <= value) * !! w_sim) / sum(!! w_sim),
      !! Fhat_C_A := sum((!! y_sim <= value) * !! w_AB) / sum(!! w_AB),
      !! Fhat_C_B := sum((!! y_sim <= value) * !! w_BA) / sum(!! w_BA),
      !! Nhat := sum(!! w_sim)
    )

  Fhat_partitions %>% dplyr::ungroup() %>% dplyr::mutate(yvalue = value)
}



#' @inheritParams nopodec_mean.reweighted
#'
#' @examples
#' r00 <- reweight_strata_all4(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' str(r00)
#' names(r00)
#' class(r00)
#'
#' nopodec_Fhat(r00, value = 5000)
#'
#' @export
#' @rdname nopodec_Fhat
nopodec_Fhat.reweighted <- function(.reweighted, value, ...){
  .reweight_strata_all <- .reweighted[[".reweight_strata_all"]]
  nopodec_Fhat.default(.reweight_strata_all, value = value)
}



#' Empirical cumulative distribution function of y for groups A and B
#' (marginals) and the number of observations.
#'
#' @param ... arguments passed to or from other methods.
#' @export
margin_Fhat <- function(...){
  UseMethod("margin_Fhat")
}

