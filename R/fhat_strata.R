

#' Estimates empirical joint frequencies of strata, in and out of the common support.
#'
#' @param .cs_strata the output of \code{\link{common_support_strata2}}
#'
#'
#' @seealso \code{\link{common_support_strata2}}, whose result is the input of this function.
#'       See \code{\link{reweight_strata2}}, that takes the result of this function as its input.
#'
#' @return a data frame with number of rows equal to the number of strata multiplied by 2 (for each group), with the following columns:
#' \itemize{
#' \item \code{strata} character vector with the combinations of the modalities of the considered variables;
#' \item \code{name} of the column used as the binary treatment;
#' \item \code{common_support} logical that indicates wether the strata is in or out the common support;
#' \item \code{Nhat} estimate of the number of individuals in the strata;
#' \item \code{fhat} estimate of the relative frequency of the strata for each group, in and out the common support;
#' \item \code{fhat_groups} estimate of the relative frequency of the strata for each group (without the distinction of in and out the common support).
#' }
#'
#' @examples
#' data(invented_wages)
#' c00 <- common_support_strata2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' fhat_strata2(c00)
#'
#' @export
fhat_strata2 <- function(.cs_strata){
  # .cs_strata: output di common_support_strata2
  weights <- attributes(.cs_strata)[["weights"]]
  treatment <- attributes(.cs_strata)[["treatment"]]

  dots <- list(lazyeval::interp(~sum(var), var = as.name(weights)))
#   treatment <- colnames(cs_strata)[1]  # fragilino... (modificato dalla versione 0.0.3 alla 0.0.4)

  dots_group <- c("strata", "common_support", treatment)

  Nhat <- NULL # to avoid a NOTE from R CMD check (...)
  fhat <- .cs_strata %>%
    dplyr::group_by_(.dots = dots_group) %>%
    dplyr::summarise_(.dots = stats::setNames(dots, c("Nhat"))) %>%
    dplyr::ungroup() #%>%
#    tidyr::complete_(cols = c("strata", treatment), fill = list(NA))

  # Impostare a FALSE dove is.na(common_support) e 0 dove is.na(Nhat)?
#   fhat$common_support[is.na(fhat$common_support)] <- FALSE
#   fhat$Nhat[is.na(fhat$Nhat)] <- 0

  fhat <- fhat %>%
    dplyr::group_by_(.dots = c(treatment, "common_support")) %>%
    dplyr::mutate(fhat = Nhat / sum(Nhat)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_(.dots = treatment) %>%
    dplyr::mutate(fhat_groups = Nhat / sum(Nhat)) %>%
    dplyr::ungroup() %>%
    tidyr::complete_(cols = c("strata", treatment), fill = list(NA))

  # Impostare a FALSE dove is.na(common_support) e 0 dove is.na(Nhat)?
  # Anche fhat_groups uguale a 0 dove is.na(fhat_groups)
    fhat$common_support[is.na(fhat$common_support)] <- FALSE
    fhat$Nhat[is.na(fhat$Nhat)] <- 0
    fhat$fhat_groups[is.na(fhat$fhat_groups)] <- 0

    fhat %>%
    arrange_(.dots = colnames(fhat)[1:2])
}


