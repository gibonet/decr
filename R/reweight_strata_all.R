

#' Estimates adjustment (counterfactual) weights in the common support and joins with the data.
#' For observations out of the common support, the weights are equal to the sample weights (or one, in case of absence of sample weights).
#'
#' @inheritParams common_support_strata2
#'
#' @seealso \code{\link{common_support_strata2}}, \code{\link{fhat_strata2}} and
#'          \code{\link{reweight_strata2}}, that are used internally by this function.
#'          For the next steps, see for example \code{\link{nopodec_mean}}, \code{\link{dec_median}}
#'          and \code{\link{dec_quantile}}.
#'          See also \code{\link{margin_mean}} and \code{\link{margin_quantile}}, that use the output
#'          of this function as their input.
#'          This function is also used in \code{\link{reweight_strata_all4}}, whose result is an object of class
#'          \code{reweighted}, that is a list with 3 components: the output of \code{\link{fhat_strata2}}
#'          (\code{.fhat_strata}), \code{\link{reweight_strata2}} (\code{.reweight_strata}) and
#'          \code{\link{reweight_strata_all2}} (\code{.reweight_strata_all}).
#'
#' @return a data frame with the same number of rows of the starting data, with columns of the chosen \code{treatment}, \code{variables}, \code{y}, \code{weights}, \code{common_support} (\code{TRUE} or \code{FALSE}), \code{strata} (a \code{strata} is given by a certain combination of the modalities of the \code{variables}), adjustment factors (\code{rw_BA} and \code{rw_AB}), empirical joint frequencies of strata in the two groups (\code{f_A} and \code{f_B}) and adjustment weights (\code{w_BA} and \code{w_AB}), which have to be used to perform (wage) decompositions
#'
#' @examples
#' data(invented_wages)
#' reweight_strata_all2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' @export
reweight_strata_all2 <- function(data, treatment, variables, y, weights = NULL){
#   .cs <- common_support2(
#     data = data, treatment = treatment, variables = variables)

  .cs_strata <- common_support_strata2(
    data = data, treatment = treatment, variables = variables,
    y = y, weights = weights)

  if(is.null(weights)) weights <- attributes(.cs_strata)[["weights"]]

  .fhat_strata <- fhat_strata2(.cs_strata = .cs_strata)

  if(is.factor(.fhat_strata[[2]])){
    groups_ <- levels(.fhat_strata[[2]])
  }else{
    groups_ <- unique(.fhat_strata[[2]])
  }

  .reweight_strata <- reweight_strata2(.fhat_strata)

  .cs_strata <- .cs_strata %>%
    dplyr::left_join(.reweight_strata, by = "strata")

  k_BA <- .cs_strata$common_support &
    .cs_strata[[treatment]] == groups_[2]
  k_AB <- .cs_strata$common_support &
    .cs_strata[[treatment]] == groups_[1]
  k_out <- !.cs_strata$common_support
  .cs_strata$rw_BA[k_AB] <- 1
  .cs_strata$rw_AB[k_BA] <- 1
  .cs_strata$rw_BA[k_out] <- 1
  .cs_strata$rw_AB[k_out] <- 1
  mut1 <- lazyeval::interp(~w * rw_BA, w = as.name(weights))
  mut2 <- lazyeval::interp(~w * rw_AB, w = as.name(weights))
  .cs_strata <- .cs_strata %>%
    mutate2_(.dots = stats::setNames(
      list(mut1, mut2), c("w_BA", "w_AB")))

  names(groups_) <- c("A", "B")
  attributes(.cs_strata)[["treatment"]] <- treatment
  attributes(.cs_strata)[["variables"]] <- variables
  attributes(.cs_strata)[["y"]] <- y
  attributes(.cs_strata)[["weights"]] <- weights
  attributes(.cs_strata)[["groups"]] <- groups_
  .cs_strata
}

# #' @rdname reweight_strata_all
# #' @export


# .cs_strata: output di common_support_strata2
reweight_strata_all3 <- function(.cs_strata){
  treatment <- attributes(.cs_strata)[["treatment"]]
  weights <- attributes(.cs_strata)[["weights"]]

  .fhat_strata <- fhat_strata2(.cs_strata = .cs_strata)

  if(is.factor(.fhat_strata[[treatment]])){
    groups_ <- levels(.fhat_strata[[treatment]])
  }else{
    groups_ <- unique(.fhat_strata[[treatment]])
  }

  .reweight_strata <- reweight_strata2(.fhat_strata)

  .cs_strata <- .cs_strata %>%
    dplyr::left_join(.reweight_strata, by = "strata")

  k_BA <- .cs_strata$common_support &
    .cs_strata[[treatment]] == groups_[2]
  k_AB <- .cs_strata$common_support &
    .cs_strata[[treatment]] == groups_[1]
  k_out <- !.cs_strata$common_support
  .cs_strata$rw_BA[k_AB] <- 1
  .cs_strata$rw_AB[k_BA] <- 1
  .cs_strata$rw_BA[k_out] <- 1
  .cs_strata$rw_AB[k_out] <- 1
  mut1 <- lazyeval::interp(~w * rw_BA, w = as.name(weights))
  mut2 <- lazyeval::interp(~w * rw_AB, w = as.name(weights))
  .cs_strata <- .cs_strata %>%
    mutate2_(.dots = stats::setNames(
      list(mut1, mut2), c("w_BA", "w_AB")))
  attributes(.cs_strata)[["weights"]] <- weights
  .cs_strata
}


# Voglio campionare (con reinserimento) nei 4 sotto-insiemi
# common_support x treatment (2 x 2 = 4)
# .cs_strata: output di common_support_strata2
resample_cs_strata <- function(.cs_strata, n = nrow(.cs_strata)){
  treatment <- attributes(.cs_strata)[["treatment"]]
  weights <- attributes(.cs_strata)[["weights"]]

  cs_strata_r <- dplyr::sample_n(.cs_strata, size = n, replace = TRUE)
  attributes(cs_strata_r)[["treatment"]] <- treatment
  attributes(cs_strata_r)[["weights"]] <- weights
  cs_strata_r
}


# Campionare con reinserimento nei 4 sotto-insiemi
# common_support x treatment (2 x 2 = 4)
# .cs_strata: output di common_support_strata2
resample_cs_strata2 <- function(.cs_strata){
  treatment <- attributes(.cs_strata)[["treatment"]]
  weights <- attributes(.cs_strata)[["weights"]]
  common_support <- "common_support"
  . <- NULL

  cs_strata_r <- .cs_strata %>%
    dplyr::group_by_(.dots = c(treatment, common_support)) %>%
    dplyr::do_(~dplyr::sample_n(., size = nrow(.), replace = TRUE)) %>%
    dplyr::ungroup()

  attributes(cs_strata_r)[["treatment"]] <- treatment
  attributes(cs_strata_r)[["weights"]] <- weights
  cs_strata_r
}



# E ricampionare per common_support x treatment x strata???)
# Campionare con reinserimento nei 4 sotto-insiemi + strata
# common_support x treatment (2 x 2 = 4) x strata = ?
# .cs_strata: output di common_support_strata2
resample_cs_strata3 <- function(.cs_strata){
  treatment <- attributes(.cs_strata)[["treatment"]]
  weights <- attributes(.cs_strata)[["weights"]]
  common_support <- "common_support"
  . <- NULL
  strata <- "strata"

  cs_strata_r <- .cs_strata %>%
    dplyr::group_by_(.dots = c(treatment, strata)) %>%
    dplyr::do_(~dplyr::sample_n(., size = nrow(.), replace = TRUE)) %>%
    dplyr::ungroup()

  attributes(cs_strata_r)[["treatment"]] <- treatment
  attributes(cs_strata_r)[["weights"]] <- weights
  cs_strata_r
}
# sembra ok ma è molto lungo (gli strati sono molti)



#' Estimates adjustment (counterfactual) weights in the common support and joins with the data.
#'
#' @description This function estimates adjustment (counterfactual) weights in the common support and joins with the data. For observations out of the common support, the weights are equal to the sample weights (or one, in case of absence of sample weights). Note that this function does the same things of \code{\link{reweight_strata_all2}}, but stores two intermediate results: the output of functions \code{\link{fhat_strata2}} and \code{\link{reweight_strata2}} (see the description in the value section).
#'
#'
#' @inheritParams reweight_strata_all2
#'
#'
#' @seealso \code{\link{fhat_strata2}}, \code{\link{reweight_strata2}}
#'         and \code{\link{reweight_strata_all2}}, that are the 3 components of the list
#'         resulting from this function (an object of class \code{reweighted}).
#'         For the next steps, see for example \code{\link{nopodec_mean}}, \code{\link{dec_median}}
#'          and \code{\link{dec_quantile}}.
#'          See also \code{\link{margin_mean}} and \code{\link{margin_quantile}}, that use the output
#'          of this function as their input.
#'
#'
#' @return An object of class \code{reweighted}. This is a list with the following elements:
#'   \item{.fhat_strata}{empirical joint frequencies of strata, in and out of the common support. This is the output of \code{\link{fhat_strata2}}}
#'   \item{.reweight_strata}{adjustment factors to estimate counterfactuals (in the common support). This is the output of \code{\link{reweight_strata2}}}
#'   \item{.reweight_strata_all}{data joined with the estimated adjustment (counterfactual) weights in the common support. This is the output of \code{\link{reweight_strata_all2}}}
#'
#' @export
#' @examples
#' data(invented_wages)
#' r00 <- reweight_strata_all4(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' str(r00)
#' names(r00)
#' class(r00)
reweight_strata_all4 <- function(data, treatment, variables, y, weights = NULL){
  #   .cs <- common_support2(
  #     data = data, treatment = treatment, variables = variables)

  .cs_strata <- common_support_strata2(
    data = data, treatment = treatment, variables = variables,
    y = y, weights = weights)

  if(is.null(weights)) weights <- attributes(.cs_strata)[["weights"]]

  .fhat_strata <- fhat_strata2(.cs_strata = .cs_strata)

  if(is.factor(.fhat_strata[[2]])){
    groups_ <- levels(.fhat_strata[[2]])
  }else{
    groups_ <- unique(.fhat_strata[[2]])
  }

  .reweight_strata <- reweight_strata2(.fhat_strata)

  .cs_strata <- .cs_strata %>%
    dplyr::left_join(.reweight_strata, by = "strata")

  k_BA <- .cs_strata$common_support &
    .cs_strata[[treatment]] == groups_[2]
  k_AB <- .cs_strata$common_support &
    .cs_strata[[treatment]] == groups_[1]
  k_out <- !.cs_strata$common_support
  .cs_strata$rw_BA[k_AB] <- 1
  .cs_strata$rw_AB[k_BA] <- 1
  .cs_strata$rw_BA[k_out] <- 1
  .cs_strata$rw_AB[k_out] <- 1
  mut1 <- lazyeval::interp(~w * rw_BA, w = as.name(weights))
  mut2 <- lazyeval::interp(~w * rw_AB, w = as.name(weights))
  .cs_strata <- .cs_strata %>%
    mutate2_(.dots = stats::setNames(
      list(mut1, mut2), c("w_BA", "w_AB")))

  names(groups_) <- c("A", "B")
  attributes(.cs_strata)[["treatment"]] <- treatment
  attributes(.cs_strata)[["variables"]] <- variables
  attributes(.cs_strata)[["y"]] <- y
  attributes(.cs_strata)[["weights"]] <- weights
  attributes(.cs_strata)[["groups"]] <- groups_
  .cs_strata

  out <- list(.fhat_strata = .fhat_strata,
              .reweight_strata = .reweight_strata,
              .reweight_strata_all = .cs_strata)
  class(out) <- "reweighted"
  out
}
