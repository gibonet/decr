

#' Establishes Common Support and joins with the data.
#'
#' @inheritParams common_support2
#' @param y name of the outcome variable for which you want to make the decomposition
#' @param weights name of the weight variable (sample weights). If \code{NULL}
#' (default value) it uses equal weights for all observations, adding
#' a column of ones
#'
#' @return a data frame with the same number of rows of the starting data, with columns of the chosen \code{treatment}, \code{variables}, \code{y}, \code{weights}, \code{common_support} (TRUE/FALSE) and \code{strata} (a strata is given by a certain combination of the modalities of the variables)
#'
#' @seealso See \code{\link{common_support2}}, which is used by this function to establish common support.
#'     See \code{\link{fhat_strata2}} and \code{\link{reweight_strata2}}, that do the next steps.
#'     See \code{\link{reweight_strata_all2}} that does all the steps of \code{\link{common_support_strata2}},
#'      \code{\link{fhat_strata2}} and \code{\link{reweight_strata2}}.
#'
#' @examples
#' data(invented_wages)
#' common_support_strata2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' @export
common_support_strata2 <- function(data, treatment, variables, y, weights = NULL){
#  treatment <- attributes(.common_support)[["treatment"]]
#  variables <- attributes(.common_support)[["variables"]]

  if(is.null(weights)){
    data <- data %>% dplyr::mutate(ones = 1L)
    weights <- "ones"
  }

  stopifnot(check_char_length_1(weights))
  stopifnot(check_char_length_1(y))

  .common_support <- common_support2(data = data, treatment = treatment,
                                     variables = variables)
  # Join dei dati con .common_support
  res <- data %>%
    dplyr::as_data_frame() %>%
    select2_(.variables = c(treatment, variables, y, weights)) %>%
    dplyr::left_join(.common_support, by = variables)
  attributes(res)[["weights"]] <- weights
  attributes(res)[["treatment"]] <- treatment
  # attributes(res)[["y"]] <- y  # prova
  # attributes(res)[["variables"]] <- variables  # prova
  res
}


# don't export for the moment
# two new arguments: breaks_fun and add2names
# performs step 1 (common_support) and 2 (common_support_strata)
# if one or more variables are of type double, it creates 'automatically'
# factor versions (with the function given as breaks_fun argument)
# #' @rdname common_support_strata2
# #' @export
common_support_strata3 <- function(data, treatment, variables,
                                   y, weights = NULL,
                                   breaks_fun = quantile_breaks, add2names = "_new"
){
  # Check if some variables are of type double
  doubles_ <- check_doubles(data %>% dplyr::select_(.dots = variables))
  # If there are no doubles, returns character(0)

  if(length(doubles_) > 0){
    breaks_fun <- match.fun(breaks_fun)
    dots_doubles <- vector(mode = "list", length = length(doubles_))
    doubles_new <- paste0(doubles_, add2names)
    for(i in seq_along(doubles_)){
      dots_doubles[[i]] <-
        lazyeval::interp(~cut_c(x, breaks = unique(breaks_fun(x))),
                         x = as.name(doubles_[i]))
    }

    # Add new columns (doubles transformed into factors)
    data <- data %>%
      dplyr::mutate_(.dots = stats::setNames(dots_doubles, doubles_new))

    which_doubles_ <- which(variables %in% doubles_)
    new_variables <- variables
    new_variables[which_doubles_] <- doubles_new
  }else{
    new_variables <- variables
    doubles_new <- character(0)
  }

  .common_support <- common_support2(data = data,
                                     treatment = treatment,
                                     variables = new_variables)
  # Fino a qui sono stati fatti i passi di common_support2
  # Da qui in avanti vanno fatti i passi di common_support_strata2

  if(is.null(weights)){
    data <- data %>% dplyr::mutate(ones = 1)
    weights <- "ones"
  }
  # Join dei dati con .common_support
  res <- data %>%
    dplyr::select_(.dots = c(treatment, variables, doubles_new, y, weights)) %>%
    dplyr::left_join(.common_support, by = new_variables)
  attributes(res)[["weights"]] <- weights
  attributes(res)[["treatment"]] <- treatment
  attributes(res)[["variables"]] <- variables
  attributes(res)[["doubles_new"]] <- doubles_new
  attributes(res)[["y"]] <- y
  res
}



vars_cs <- function(.common_support){
  # Per estrarre le "variables" dall'output di common_support2
  # La posso usare in common_support_strata2
  k <- colnames(.common_support)
  k_last <- length(k)
  k_last <- (-3:0) + k_last
  k[-c(1, k_last)]  # Toglie la prima e le ultime quattro colonne
}
