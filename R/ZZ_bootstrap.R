
# Una closure che crea la funzione da passare a boot
fun_boot_mean <- function(y, counterfactual = c("AB", "BA")){
  function(d, i){
    treatment <- attributes(d)[["treatment"]]
    weights <- attributes(d)[["weights"]]

    d <- d[i, ]
    attributes(d)[["weights"]] <- weights
    attributes(d)[["treatment"]] <- treatment

    rtmp <- reweight_strata_all3(d)
    # y <- attributes(rtmp)[["y"]]  # PROVA

    stmp <- nopodec_mean(rtmp, y = y, weights = weights)

    unlist(nopodec(stmp, counterfactual = counterfactual[1]))
  }
}

# #' @inheritParams reweight_strata_all2


#' Bootstrap of the mean decomposition procedure.
#'
#' Bootstrap estimates of \code{\link{nopodec_mean}} and \code{\link{nopodec}}.
#' This serves to estimate for example confidence intervals of each component
#' of the decomposition.
#'
#' @param data data frame
#' @param treatment column name of the binary variable
#' @param variables character vector of the variables' names, for which the common support has to be established
#' @param y name of the outcome variable for which you want to make the decomposition
#' @param weights name of the weight variable (sample weights). If \code{NULL}
#' (default value) it uses equal weights for all observations, adding
#' a column of ones
#' @param R The number of bootstrap replicates. This will be a single positive integer.
#' @param counterfactual "AB" or "BA". "AB" means that we want to estimate the counterfactual (wage) of group A, as if their characteristics were distributed as in group B. "BA" is the opposite (characteristics of group B are balanced to those of group A).
#' @param ... other arguments passed on to \code{\link{boot}}.
#'
#' @examples
#' \dontrun{
#' data(invented_wages)
#' b01 <- boot_nopodec_mean(data = invented_wages, treatment = "gender",
#'                         variables = c("sector", "education"), y = "wage",
#'                         R = 500)
#' }
#'
#' @export
boot_nopodec_mean <- function(data, treatment, variables, y, weights = NULL, R = 10, counterfactual = c("AB", "BA"), ...){
  c01 <- common_support_strata2(data, treatment, variables, y, weights)
  dots_mutate <- lazyeval::interp(~paste(x1, x2, sep = "_"),
                                  x1 = as.name("strata"),
                                  x2 = as.name(treatment))
  c01 <- c01 %>%
    mutate2_(.dots = stats::setNames(list(dots_mutate), c("strata2")))
  attributes(c01)[["weights"]] <- weights
  attributes(c01)[["treatment"]] <- treatment

  f <- fun_boot_mean(y = y, counterfactual = counterfactual)

  boot::boot(c01, f, R = R, stype = "i", strata = as.factor(c01[["strata2"]]), ...)  # c01$strata2
}



# Una closure che crea la funzione da passare a boot
fun_boot_quantile <- function(y, probs = 0.5, counterfactual = c("AB", "BA")){
  function(data, i){
    treatment <- attributes(data)[["treatment"]]
    weights <- attributes(data)[["weights"]]

    d <- data[i, ]
    attributes(d)[["weights"]] <- weights
    attributes(d)[["treatment"]] <- treatment

    rtmp <- reweight_strata_all3(d)

    stmp <- dec_quantile(rtmp, y = y, weights = weights, probs = probs)

    unlist(dec_(stmp, counterfactual = counterfactual[1]))[-1]
  }
}


#' Bootstrap of the quantile decomposition procedure.
#'
#' Bootstrap estimates of \code{\link{dec_quantile}} and \code{\link{dec_}}.
#' This serves to estimate for example confidence intervals of each component
#' of the decomposition.
#'
#' @inheritParams boot_nopodec_mean
#' @param probs numeric vector of length one with the desired quantile level (should be between 0 and 1).
#'
#' @examples
#' \dontrun{
#' data(invented_wages)
#' b01 <- boot_dec_quantile(data = invented_wages, treatment = "gender",
#'                         variables = c("sector", "education"), y = "wage",
#'                         R = 500, probs = 0.5)
#' }
#'
#' @export
boot_dec_quantile <- function(data, treatment, variables, y, weights = NULL, R = 10, probs = 0.5, counterfactual = c("AB", "BA"), ...){
  c01 <- common_support_strata2(data, treatment, variables, y, weights)
  dots_mutate <- lazyeval::interp(~paste(x1, x2, sep = "_"),
                                  x1 = as.name("strata"),
                                  x2 = as.name(treatment))
  c01 <- c01 %>%
    mutate2_(.dots = stats::setNames(list(dots_mutate), c("strata2")))
  attributes(c01)[["weights"]] <- weights
  attributes(c01)[["treatment"]] <- treatment

  f <- fun_boot_quantile(y = y, probs = probs, counterfactual = counterfactual)

  boot::boot(c01, f, R = R, stype = "i", strata = as.factor(c01[["strata2"]]), ...)
}



# Una closure che crea la funzione da passare a boot
fun_boot_quantiles <- function(y, probs = c(0.25, 0.5, 0.75), counterfactual = c("AB", "BA")){
  function(data, i){
    treatment <- attributes(data)[["treatment"]]
    weights <- attributes(data)[["weights"]]
    
    d <- data[i, ]
    attributes(d)[["weights"]] <- weights
    attributes(d)[["treatment"]] <- treatment
    
    rtmp <- reweight_strata_all3(d)
    
    stmp <- dec_quantiles(rtmp, y = y, weights = weights, probs = probs)
    stmp_dec <- dec_all_(stmp, counterfactual = counterfactual[1])
    
    # Via la prima colonna (con i livelli dei quantili)
    stmp_dec <- stmp_dec[ , -1, drop = FALSE]
    
    # Trasforma il data frame in un vettore
    stmp_dec_vector <- unlist(stmp_dec)
    stmp_dec_vector
  }
}


boot_dec_quantiles <- function(data, treatment, variables, y, weights = NULL, 
                               R = 10, probs = c(0.25, 0.5, 0.75), 
                               counterfactual = c("AB", "BA"), ...){
  c01 <- common_support_strata2(data, treatment, variables, y, weights)
  dots_mutate <- lazyeval::interp(~paste(x1, x2, sep = "_"),
                                  x1 = as.name("strata"),
                                  x2 = as.name(treatment))
  c01 <- c01 %>%
    mutate2_(.dots = stats::setNames(list(dots_mutate), c("strata2")))
  attributes(c01)[["weights"]] <- weights
  attributes(c01)[["treatment"]] <- treatment
  
  f <- fun_boot_quantiles(y = y, probs = probs, counterfactual = counterfactual)
  
  boot::boot(c01, f, R = R, stype = "i", strata = as.factor(c01[["strata2"]]), ...)
}
