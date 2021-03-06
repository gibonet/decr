
#' Estimates medians of the y variable for the two groups, in and out the common support.
#' In the common support, counterfactual medians of y are estimated.
#' It also estimates the number of individuals of the two groups.
#' Moreover, marginal medians of the two groups are also computed.
#'
#' The results in the common support are all the components necessary
#' to perform a decomposition of the median wage difference between
#' two groups, in two components: one that can be explained by the
#' difference in the distributions of characteristics between the two groups
#' (delta_X), and one that cannot be explained by the different
#' characteristics of the two groups (delta_S).
#'
#' @param ... arguments passed to or from other methods.
#' @export
dec_median <- function(...){
  UseMethod("dec_median")
}


#' @inheritParams nopodec_mean
#'
#' @return A data frame with four, five or six rows, with the following columns:
#' \itemize{
#' \item the name of the treatment column used in \code{\link{reweight_strata_all2}};
#' \item \code{common_support} logical indicating if in or out the common support;
#' \item \code{yhat} median of the y variable, weighted by the given weights;
#' \item \code{yhat_C_A} counterfactual median y of group A as if they had the same distribution of characteristics of group B. This is computed in the common support only and for group A individuals. It is computed with the weights \code{w_AB} that result from \code{\link{reweight_strata_all2}};
#' \item \code{yhat_C_B} counterfactual median y of group B as if they had the same distribution of characteristics of group A. This is computed in the common support only and for group B individuals. It is computed with the weights \code{w_BA} that result from \code{\link{reweight_strata_all2}};
#' \item \code{Nhat} estimate of the number of individuals.
#' \item \code{probs} the level of the estimated quantile. In this case, it is equal to 0.5 (the median).
#' }
#' The number of rows is given by the combinations of the distinct values of
#' the first two columns: \code{treatment} and \code{common_support}.
#' In addition to these rows, there are two more rows at the end, with the
#' marginal medians of the two groups.
#' In the "typical" case, the resulting data frame will have 4 rows. It can have
#' three rows if all the individuals of one group are in the common support.
#' In case of no common support or no out-of-support, the data frame will have two rows.
#'
#'
#' @examples
#' data(invented_wages)
#'
#' # Common support and computation of counterfactual weights
#' r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' # Computation of the elements necessary to the decomposition
#' dec_median(r00)
#'
#' @rdname dec_median
#' @export
dec_median.default <- function(.reweight_strata_all, y = NULL, weights = NULL, ...){
  dec_quantile.default(.reweight_strata_all, y = y, weights = weights, probs = 0.5)
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
#' dec_median(r00)
#'
#' @export
#' @rdname dec_median
dec_median.reweighted <- function(.reweighted, ...){
  .reweight_strata_all <- .reweighted[[".reweight_strata_all"]]
  dec_median.default(.reweight_strata_all, ...)
}




#' Estimates quantiles of the y variable for the two groups, in and out the common support.
#' In the common support, counterfactual quantiles of y are estimated.
#' It also estimates the number of individuals of the two groups.
#' Moreover, marginal quantiles of the two groups are also computed.
#'
#' The results in the common support are all the components necessary
#' to perform a decomposition of the quantile wage difference between
#' two groups, in two components: one that can be explained by the
#' difference in the distributions of characteristics between the two groups
#' (delta_X), and one that cannot be explained by the different
#' characteristics of the two groups (delta_S).
#'
#' @param ... arguments passed to or from other methods.
#' @export
dec_quantile <- function(...){
  UseMethod("dec_quantile")
}


#' @inheritParams nopodec_mean
#' @param probs numeric vector of length one with the desired quantile level (should be between 0 and 1).
#'
#' @return A data frame with four, five or six rows, with the following columns:
#' \itemize{
#' \item the name of the treatment column used in \code{\link{reweight_strata_all2}};
#' \item \code{common_support} logical indicating if in or out the common support;
#' \item \code{yhat} quantile of the y variable, weighted by the given weights;
#' \item \code{yhat_C_A} counterfactual quantile y of group A as if they had the same distribution of characteristics of group B. This is computed in the common support only and for group A individuals. It is computed with the weights \code{w_AB} that result from \code{\link{reweight_strata_all2}};
#' \item \code{yhat_C_B} counterfactual quantile y of group B as if they had the same distribution of characteristics of group A. This is computed in the common support only and for group B individuals. It is computed with the weights \code{w_BA} that result from \code{\link{reweight_strata_all2}};
#' \item \code{Nhat} estimate of the number of individuals.
#' \item \code{probs} the level of the estimated quantile. Should be a number between zero and one (default: 0.5: the median).
#' }
#' The number of rows is given by the combinations of the distinct values of
#' the first two columns: \code{treatment} and \code{common_support}.
#' In addition to these rows, there are two more rows at the end, with the
#' marginal quantiles of the two groups.
#' In the "typical" case, the resulting data frame will have 4 rows. It can have three rows if all the individuals of one group are in the common support.
#' In case of no common support or no out-of-support, the data frame will have two rows.
#'
#'
#' @examples
#' data(invented_wages)
#'
#' # Common support and computation of counterfactual weights
#' r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' # Computation of the elements necessary to the decomposition
#' dec_quantile(r00)
#' dec_quantile(r00, probs = 0.75)
#'
#' @rdname dec_quantile
#' @export
dec_quantile.default <- function(.reweight_strata_all, y = NULL, weights = NULL, probs = 0.5, ...){
  stopifnot(length(probs) == 1)

  treatment <- attributes(.reweight_strata_all)[["treatment"]]
  if(is.null(weights)) weights <- attributes(.reweight_strata_all)[["weights"]]
  if(is.null(y)) y <- attributes(.reweight_strata_all)[["y"]]

  # Preparazioni per summarise_
  # yhat_marginals <- lazyeval::interp(~wq(x, w, probs = probs),
  #                                    x = as.name(y), w = as.name(weights))
  # yhat_counterfactual_A <- lazyeval::interp(~wq(x, w_AB, probs = probs),
  #                                           x = as.name(y))
  # yhat_counterfactual_B <- lazyeval::interp(~wq(x, w_BA, probs = probs),
  #                                           x = as.name(y))
  # nhat <- lazyeval::interp(~sum(w), w = as.name(weights))
  y_sim <- rlang::sym(y); w_sim <- rlang::sym(weights)
  w_AB <- rlang::sym("w_AB"); w_BA <- rlang::sym("w_BA")
  y_sim <- rlang::enquo(y_sim); w_sim <- rlang::enquo(w_sim)
  w_AB <- rlang::enquo(w_AB); w_BA <- rlang::enquo(w_BA)

  yhat <- "yhat"; yhat_C_A <- "yhat_C_A"; yhat_C_B <- "yhat_C_B"; Nhat <- "Nhat"

  # Quantili partitions
  quantiles_partitions <- .reweight_strata_all %>%
    gby_(c(treatment, "common_support")) %>%
    # summarise2_(.dots = stats::setNames(
    #   list(yhat_marginals, yhat_counterfactual_A, yhat_counterfactual_B, nhat),
    #   c("yhat", "yhat_C_A", "yhat_C_B", "Nhat")))
    dplyr::summarise(
      !! yhat := wq(!! y_sim, !! w_sim, probs = probs),
      !! yhat_C_A := wq(!! y_sim, !! w_AB, probs = probs),
      !! yhat_C_B := wq(!! y_sim, !! w_BA, probs = probs),
      !! Nhat := sum(!! w_sim)
    )

  # Quantili marginali
#   quantiles_marginali <- .reweight_strata_all %>%
#     dplyr::group_by_(.dots = treatment) %>%
#     dplyr::summarise_(.dots = stats::setNames(
#       list(yhat_marginals, nhat),
#       c("yhat", "Nhat")))

  quantiles_partitions <- quantiles_partitions %>%
    dplyr::ungroup() %>%
    dplyr::bind_cols(data.frame(probs = rep(probs, nrow(quantiles_partitions))))

  # dplyr::bind_rows(margin_quantile(.reweight_strata_all, probs = probs)) %>% ## PROVA!!! ##
  marginal_quantiles <- margin_quantile(.reweight_strata_all, y = y, weights = weights, probs = probs)
  quantiles_partitions <- quantiles_partitions %>%
    dplyr::bind_rows(marginal_quantiles)

  attributes(quantiles_partitions)[["treatment"]] <- attributes(.reweight_strata_all)[["treatment"]]
  attributes(quantiles_partitions)[["variables"]] <- attributes(.reweight_strata_all)[["variables"]]
  attributes(quantiles_partitions)[["y"]] <- y
  attributes(quantiles_partitions)[["weights"]] <- weights
  attributes(quantiles_partitions)[["groups"]] <- attributes(.reweight_strata_all)[["groups"]]
  attributes(quantiles_partitions)[["probs"]] <- probs
  quantiles_partitions
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
#' dec_median(r00)
#'
#' @export
#' @rdname dec_quantile
dec_quantile.reweighted <- function(.reweighted, ...){
  .reweight_strata_all <- .reweighted[[".reweight_strata_all"]]
  dec_quantile.default(.reweight_strata_all, ...)
}






#' Performs decomposition of the quantile difference (of y) between two groups (in 2 components) in the common support.
#'
#' @param .dec_ output of \code{\link{dec_median}} or \code{\link{dec_quantile}}.
#' @param counterfactual "AB" or "BA". "AB" means that we want to estimate the counterfactual (wage) of group A, as if their characteristics were distributed as in group B. "BA" is the opposite (characteristics of group B are balanced to those of group A).
#'
#' @return a list with four components:
#'
#' \itemize{
#'   \item \code{probs}: the chosen quantile level;
#'   \item \code{delta_tot}: total observed difference between the quantiles (of wages)
#'    of group A and B (all the sample);
#'   \item \code{delta_tot_CS}: total observed difference between the quantiles (of wages)
#'    of group A and B in the common support;
#'   \item \code{delta_AB}: difference explained by the fact that the two groups
#'    have some combinations of characteristics that the other group has not.
#'   \item \code{delta_X}: part explained by the fact that the two groups have a
#'    different distribution of characteristics (same combinations of characteristics)
#'    but distributed differently);
#'   \item \code{delta_S}: part not justified by the different distributions of the
#'    characteristics of the two groups, and potentially due to a difference
#'     in the remuneration structures between the two groups.
#' }
#'
#' @examples
#' data(invented_wages)
#'
#' # Common support and computation of counterfactual weights
#' r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' # Computation of the elements necessary to the decomposition
#' d00 <- dec_quantile(r00)
#'
#' # Decomposition of the difference of the medians of the two groups
#' # (in the common support) in two components
#' dec_(d00)
#'
#' @export
dec_ <- function(.dec_, counterfactual = c("AB", "BA")){
  stopifnot(counterfactual[1] %in% c("AB", "BA"))
  common_support <- "common_support"
  .dec_cs <- .dec_ %>% filter2_(.dots = common_support)

  treatment <- attributes(.dec_)[["treatment"]]
  probs <- attributes(.dec_)[["probs"]]
  if(is.factor(.dec_cs[[treatment]])){
    groups_ <- levels(.dec_cs[[treatment]])
  }else{
    groups_ <- unique(.dec_cs[[treatment]])
  }

  # sel_A <- lazyeval::interp(~x == y, x = as.name(treatment), y = groups_[1])
  # sel_B <- lazyeval::interp(~x == y, x = as.name(treatment), y = groups_[2])

  sel_A <- paste0(treatment, " == ", "\"", groups_[1], "\"")
  sel_B <- paste0(treatment, " == ", "\"", groups_[2], "\"")

  # perc_ <- lazyeval::interp(~x / sum(x), x = as.name("Nhat"))

  yhat_A_in <- (.dec_cs %>% filter2_(sel_A))$yhat
  yhat_B_in <- (.dec_cs %>% filter2_(sel_B))$yhat

  # I due controfattuali (per una scomposizione ne serve uno dei due, ma la scomposizione
  # può essere fatta con entrambi: X_A'Beta_B e X_B'Beta_A)

  # Salario del gruppo A come se avesse le stesse caratteristiche del gruppo B (X_B'Beta_A)
  yhat_AB_C <- (.dec_cs %>% filter2_(sel_A))$yhat_C_A

  # Salario del gruppo B come se avesse le stesse caratteristiche del gruppo A (X_A'Beta_B)
  yhat_BA_C <- (.dec_cs %>% filter2_(sel_B))$yhat_C_B

  counterfactual <- match.arg(counterfactual)
  # Prima scomposizione: con yhat_AB_C
  if(counterfactual == "AB"){
    delta_X <- yhat_A_in - yhat_AB_C
    delta_S <- yhat_AB_C - yhat_B_in
  }
  if(counterfactual == "BA"){
    delta_S <- yhat_A_in - yhat_BA_C
    delta_X <- yhat_BA_C - yhat_B_in
  }

  if(nrow(.dec_cs) == 0L && check_numeric_0(delta_S) &&
     check_numeric_0(delta_X)){
    delta_S <- 0
    delta_X <- 0
    message("Note that in this case there is not common support between the characteristics of the two groups.")
  }

  delta_tot_CS <- delta_X + delta_S
  # .dec_marginal <- .dec_ %>% filter2_(.dots = ~is.na(common_support))
  .dec_marginal <- .dec_ %>% filter2_("is.na(common_support)")
  yhat_A <- (.dec_marginal %>% filter2_(sel_A))$yhat
  yhat_B <- (.dec_marginal %>% filter2_(sel_B))$yhat
  delta_tot <- yhat_A - yhat_B

  delta_AB <- delta_tot - delta_tot_CS

  list(probs = probs, delta_tot = delta_tot, delta_tot_CS = delta_tot_CS,
       delta_AB = delta_AB, delta_X = delta_X, delta_S = delta_S)
}




#' Estimates quantiles (of level probs) of y for groups A and B (marginals) and the number of observations.
#'
#' @param ... arguments passed to or from other methods.
#' @export
margin_quantile <- function(...){
  UseMethod("margin_quantile")
}




#' @inheritParams dec_quantile
#'
#'
#' @return a data frame with two rows (one for each group) and the follwing four columns:
#' \itemize{
#' \item the name of the treatment column used in \code{\link{reweight_strata_all2}};
#' \item \code{yhat}: quantile of the y variable;
#' \item \code{Nhat}: estimate of the number of individuals;
#' \item \code{probs}: level of the estimated quantile (between 0 and 1).
#' }
#' @examples
#' data(invented_wages)
#'
#' r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
#'                             variables = c("sector", "education"),
#'                             y = "wage", weights = "sample_weights")
#'
#' margin_quantile(r00, probs = 0.75)
#'
#' @rdname margin_quantile
#' @export
margin_quantile.default <- function(.reweight_strata_all, y = NULL, weights = NULL, probs = 0.5, ...){
  stopifnot(length(probs) == 1)

  treatment <- colnames(.reweight_strata_all)[1]
  if(is.null(weights)) weights <- attributes(.reweight_strata_all)[["weights"]]
  if(is.null(y)) y <- attributes(.reweight_strata_all)[["y"]]

  # Preparazioni per summarise_
  # yhat_marginals <- lazyeval::interp(~wq(x, w, probs = probs),
  #                                    x = as.name(y), w = as.name(weights))
  #
  # nhat <- lazyeval::interp(~sum(w), w = as.name(weights))
  y_sim <- rlang::sym(y); w_sim <- rlang::sym(weights)
  y_sim <- rlang::enquo(y_sim); w_sim <- rlang::enquo(w_sim)

  yhat <- "yhat"; Nhat <- "Nhat"

  # Quantili marginali
  quantiles_marginali <- .reweight_strata_all %>%
    gby_(treatment) %>%
    # summarise2_(.dots = stats::setNames(
    #   list(yhat_marginals, nhat),
    #   c("yhat", "Nhat")))
    dplyr::summarise(
      !! yhat := wq(!! y_sim, !! w_sim, probs = probs),
      !! Nhat := sum(!! w_sim)
    )


  quantiles_marginali <- quantiles_marginali %>%
    dplyr::ungroup() %>%
    dplyr::bind_cols(data.frame(probs = rep(probs, nrow(quantiles_marginali))))

  attributes(quantiles_marginali)[["treatment"]] <- attributes(.reweight_strata_all)[["treatment"]]
  attributes(quantiles_marginali)[["variables"]] <- attributes(.reweight_strata_all)[["variables"]]
  attributes(quantiles_marginali)[["y"]] <- y
  attributes(quantiles_marginali)[["weights"]] <- weights
  attributes(quantiles_marginali)[["groups"]] <- attributes(.reweight_strata_all)[["groups"]]
  attributes(quantiles_marginali)[["probs"]] <- probs
  quantiles_marginali
}


#' @inheritParams margin_mean.reweighted
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
#' margin_quantile(r00)
#'
#' @export
#' @rdname margin_quantile
margin_quantile.reweighted <- function(.reweighted, ...){
  .reweight_strata_all <- .reweighted[[".reweight_strata_all"]]
  margin_quantile.default(.reweight_strata_all, ...)
}



#' Computes observed (marginal) difference between statistics of two groups (mean, quantiles, ...)
#'
#'
#'
#' @param ... arguments passed to or from other methods.
#' @export
margin_difference <- function(...){
  UseMethod("margin_difference")
}


# Computes observed (marginal) difference between statistics of two groups (mean, quantiles, ...)

#' @param .margin_stat data frame with the marginal statistics of two groups. This is the output of function \code{\link{margin_mean}} or \code{\link{margin_quantile}}.
#'
#' @examples
#' data(invented_wages)
#'
#' r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
#'                             variables = c("sector", "education"),
#'                             y = "wage", weights = "sample_weights")
#'
#' m00 <- margin_mean(r00)
#'
#' m00
#'
#' # Difference between observed average wages of men and women
#' margin_difference(m00)
#'
#'
#' # An example with quantiles. In this case, the 75th percentile
#' q00 <- margin_quantile(r00, probs = 0.75)
#'
#' q00
#'
#' # Difference between observed 75th percentiles of wages of men and women
#' margin_difference(q00)
#'
#'
#' @export
#' @rdname margin_difference
margin_difference.default <- function(.margin_stat, ...){
  .margin_stat[[2]][1] - .margin_stat[[2]][2]
}


# A tibble: 2 x 3
# gender     ybar  Nhat
# <fctr>    <dbl> <dbl>
#   1    men 5323.053 42363
# 2  women 3613.789 27833
# > str(m00)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	2 obs. of  3 variables:
#   $ gender: Factor w/ 2 levels "men","women": 1 2
# $ ybar  : num  5323 3614
# $ Nhat  : num  42363 27833
# - attr(*, "treatment")= chr "gender"
# - attr(*, "variables")= chr  "sector" "education"
# - attr(*, "y")= chr "wage"
# - attr(*, "weights")= chr "sample_weights"
# - attr(*, "groups")= Named chr  "men" "women"
# ..- attr(*, "names")= chr  "A" "B"




#' Estimates quantiles of the y variable for the two groups, in and out the common support.
#' In the common support, counterfactual quantiles of y are estimated.
#' It also estimates the number of individuals of the two groups.
#' Moreover, marginal quantiles of the two groups are also computed.
#'
#' The results in the common support are all the components necessary
#' to perform a decomposition of the quantile wage difference between
#' two groups, in two components: one that can be explained by the
#' difference in the distributions of characteristics between the two groups
#' (delta_X), and one that cannot be explained by the different
#' characteristics of the two groups (delta_S).
#'
#' Note that this function estimates quantiles at different levels
#' simultaneously, while \code{\link{dec_quantile}} does only one quantile
#' level.
#'
#' @param ... arguments passed to or from other methods.
#' @export
dec_quantiles <- function(...){
  UseMethod("dec_quantiles")
}


#' @inheritParams nopodec_mean
#' @param probs numeric vector with the desired quantile levels (all the values should be between 0 and 1). Default value: \code{c(0.25, 0.5, 0.75)}
#'
#' @return A data frame with four, five or six rows, with the following columns:
#' \itemize{
#' \item the name of the treatment column used in \code{\link{reweight_strata_all2}};
#' \item \code{common_support} logical indicating if in or out the common support;
#' \item \code{yhat} quantiles of the y variable, weighted by the given weights. This is a list where each element is a numeric vector with the estimates of the quantiles at all the levels given by the argument \code{probs};
#' \item \code{yhat_C_A} counterfactual quantiles y of group A as if they had the same distribution of characteristics of group B. This is computed in the common support only and for group A individuals. It is computed with the weights \code{w_AB} that result from \code{\link{reweight_strata_all2}}. This is a list where each element is a numeric vector with the estimates of the quantiles at all the levels given by the argument \code{probs};
#' \item \code{yhat_C_B} counterfactual quantiles y of group B as if they had the same distribution of characteristics of group A. This is computed in the common support only and for group B individuals. It is computed with the weights \code{w_BA} that result from \code{\link{reweight_strata_all2}}. This is a list where each element is a numeric vector with the estimates of the quantiles at all the levels given by the argument \code{probs};
#' \item \code{Nhat} estimate of the number of individuals. This is a list where each element is a numeric vector of length one.
#' }
#' The number of rows is given by the combinations of the distinct values of
#' the first two columns: \code{treatment} and \code{common_support}.
#' In addition to these rows, there are two more rows at the end, with the
#' marginal quantiles of the two groups.
#' In the "typical" case, the resulting data frame will have 4 rows. It can have three rows if all the individuals of one group are in the common support.
#' In case of no common support or no out-of-support, the data frame will have two rows.
#'
#'
#' @examples
#' data(invented_wages)
#'
#' # Common support and computation of counterfactual weights
#' r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' # Computation of the elements necessary to the decomposition
#' dec_quantiles(r00)
#' dec_quantiles(r00, probs = seq(0.05, 0.95, by = 0.05))
#'
#' @rdname dec_quantiles
#' @export
dec_quantiles.default <- function(.reweight_strata_all, y = NULL,
                                  weights = NULL, probs = c(0.25, 0.5, 0.75), ...){
  stopifnot(probs > 0 & probs < 1)
  treatment <- attributes(.reweight_strata_all)[["treatment"]]
  if (is.null(weights))
    weights <- attributes(.reweight_strata_all)[["weights"]]
  if (is.null(y))
    y <- attributes(.reweight_strata_all)[["y"]]

  . <- NULL
  quantiles_partitions <- .reweight_strata_all %>%
    gby_(c(treatment, "common_support")) %>%
    # dplyr::do_(
    #   yhat = ~wq(.[[y]], .[[weights]], probs = probs),
    #   yhat_C_A = ~wq(.[[y]], .[["w_AB"]], probs = probs),
    #   yhat_C_B = ~wq(.[[y]], .[["w_BA"]], probs = probs),
    #   Nhat = ~sum(.[[weights]])
    # ) %>% dplyr::ungroup()
    dplyr::do(
      yhat = wq(.[[y]], .[[weights]], probs = probs),
      yhat_C_A = wq(.[[y]], .[["w_AB"]], probs = probs),
      yhat_C_B = wq(.[[y]], .[["w_BA"]], probs = probs),
      Nhat = sum(.[[weights]])
    ) %>% dplyr::ungroup()


  marginal_quantiles <- .reweight_strata_all %>%
    gby_(treatment) %>%
    # dplyr::do_(
    #   yhat = ~wq(.[[y]], .[[weights]], probs = probs),
    #   Nhat = ~sum(.[[weights]])
    # ) %>% dplyr::ungroup()
    dplyr::do(
      yhat = wq(.[[y]], .[[weights]], probs = probs),
      Nhat = sum(.[[weights]])
    ) %>% dplyr::ungroup()

  quantiles_partitions <- quantiles_partitions %>% dplyr::bind_rows(marginal_quantiles)

  attributes(quantiles_partitions)[["treatment"]] <- attributes(.reweight_strata_all)[["treatment"]]
  attributes(quantiles_partitions)[["variables"]] <- attributes(.reweight_strata_all)[["variables"]]
  attributes(quantiles_partitions)[["y"]] <- y
  attributes(quantiles_partitions)[["weights"]] <- weights
  attributes(quantiles_partitions)[["groups"]] <- attributes(.reweight_strata_all)[["groups"]]
  attributes(quantiles_partitions)[["probs"]] <- probs
  quantiles_partitions
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
#' dec_quantiles(r00)
#'
#' @export
#' @rdname dec_quantiles
dec_quantiles.reweighted <- function(.reweighted, ...){
  .reweight_strata_all <- .reweighted[[".reweight_strata_all"]]
  dec_quantiles.default(.reweight_strata_all, ...)
}



# .dec_: output of dec_quantiles

#' Performs decomposition of the quantile differences (of y) between two groups (in 2 components) in the common support.
#'
#' @param .dec_ output of \code{\link{dec_quantiles}}.
#' @param counterfactual "AB" or "BA". "AB" means that we want to estimate the counterfactual (wage) of group A, as if their characteristics were distributed as in group B. "BA" is the opposite (characteristics of group B are balanced to those of group A).
#'
#' @return a data frame with the following columns:
#'
#' \itemize{
#'   \item \code{probs}: the chosen quantile levels;
#'   \item \code{delta_tot}: total observed difference between the quantiles (of wages)
#'    of group A and B (all the sample);
#'   \item \code{delta_tot_CS}: total observed difference between the quantiles (of wages)
#'    of group A and B in the common support;
#'   \item \code{delta_AB}: difference explained by the fact that the two groups
#'    have some combinations of characteristics that the other group has not.
#'   \item \code{delta_X}: part explained by the fact that the two groups have a
#'    different distribution of characteristics (same combinations of characteristics)
#'    but distributed differently);
#'   \item \code{delta_S}: part not justified by the different distributions of the
#'    characteristics of the two groups, and potentially due to a difference
#'     in the remuneration structures between the two groups.
#' }
#'
#' @examples
#' data(invented_wages)
#'
#' # Common support and computation of counterfactual weights
#' r00 <- reweight_strata_all2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' # Computation of the elements necessary to the decompositions
#' d00 <- dec_quantiles(r00)
#'
#' # Decomposition of the difference of the medians of the two groups
#' # (in the common support) in two components
#' dec_all_(d00)
#'
#' @export
dec_all_ <- function(.dec_, counterfactual = c("AB", "BA")){
  stopifnot(counterfactual[1] %in% c("AB", "BA"))
  common_support <- "common_support"
  .dec_cs <- .dec_ %>% filter2_(.dots = common_support)
  treatment <- attributes(.dec_)[["treatment"]]
  probs <- attributes(.dec_)[["probs"]]
  if(is.factor(.dec_cs[[treatment]])) {
    groups_ <- levels(.dec_cs[[treatment]])
  }
  else{
    groups_ <- unique(.dec_cs[[treatment]])
  }
  # sel_A <- lazyeval::interp(~x == y, x = as.name(treatment),
  #                           y = groups_[1])
  # sel_B <- lazyeval::interp(~x == y, x = as.name(treatment),
  #                           y = groups_[2])
  sel_A <- paste0(treatment, " == ", "\"", groups_[1], "\"")
  sel_B <- paste0(treatment, " == ", "\"", groups_[2], "\"")

  # perc_ <- lazyeval::interp(~x/sum(x), x = as.name("Nhat"))
  yhat_A_in <- (.dec_cs %>% filter2_(sel_A))$yhat %>% lapply(function(x) as.data.frame(t(x))) %>% unlist()
  yhat_B_in <- (.dec_cs %>% filter2_(sel_B))$yhat %>% lapply(function(x) as.data.frame(t(x))) %>% unlist()
  yhat_AB_C <- (.dec_cs %>% filter2_(sel_A))$yhat_C_A %>% lapply(function(x) as.data.frame(t(x))) %>% unlist()
  yhat_BA_C <- (.dec_cs %>% filter2_(sel_B))$yhat_C_B %>% lapply(function(x) as.data.frame(t(x))) %>% unlist()
  counterfactual <- match.arg(counterfactual)
  if(counterfactual == "AB"){
    delta_X <- yhat_A_in - yhat_AB_C
    delta_S <- yhat_AB_C - yhat_B_in
  }
  if(counterfactual == "BA"){
    delta_S <- yhat_A_in - yhat_BA_C
    delta_X <- yhat_BA_C - yhat_B_in
  }
  if(nrow(.dec_cs) == 0L && check_numeric_0(delta_S) && check_numeric_0(delta_X)) {
    delta_S <- 0
    delta_X <- 0
    message("Note that in this case there is not common support between the characteristics of the two groups.")
  }
  delta_tot_CS <- delta_X + delta_S

  .dec_marginal <- .dec_ %>% filter2_(.dots = ~is.na(common_support))
  yhat_A <- (.dec_marginal %>% filter2_(sel_A))$yhat %>% lapply(function(x) as.data.frame(t(x))) %>% unlist()
  yhat_B <- (.dec_marginal %>% filter2_(sel_B))$yhat %>% lapply(function(x) as.data.frame(t(x))) %>% unlist()
  delta_tot <- yhat_A - yhat_B
  delta_AB <- delta_tot - delta_tot_CS

  # list(probs = probs, delta_tot = delta_tot, delta_tot_CS = delta_tot_CS,
  #      delta_AB = delta_AB, delta_X = delta_X, delta_S = delta_S)
  data.frame(probs = probs, delta_tot = delta_tot, delta_tot_CS = delta_tot_CS,
             delta_AB = delta_AB, delta_X = delta_X, delta_S = delta_S)
}

