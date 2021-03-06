


#' Computes the adjustment factors to estimate counterfactuals (in the common support).
#'
#' Adjustment factors are used to reweight the joint distribution of characteristics of
#' one group so that it will be equal to that of the other group
#' (both counterfacuals, AB and BA...). This will be done on the
#' common support only.
#'
#' @param .fhat_strata output of \code{\link{fhat_strata2}}
#'
#' @seealso \code{\link{fhat_strata2}}, whose result is the input of this function.
#'        See \code{\link{reweight_strata_all2}}, that uses internally functions
#'        \code{\link{common_support_strata2}}, \code{\link{fhat_strata2}} and \code{\link{reweight_strata2}}.
#'
#' @return a data frame with the \code{strata} in the common support (in the rows), the reweighting factors \code{rw_BA} and \code{rw_AB} (see below), and joint frequencies of the strata in the two groups (\code{f_A} and \code{f_B}).
#'
#' Details about the columns of the returned data frame:
#' \item{strata}{Unique combinations of the modalities of the variables used in \code{\link{common_support_strata2}} (and \code{\link{common_support2}}). In this function, the strata are only those on the common support (that is, where there exists at least one observation of each group - A and B).}
#' \item{rw_BA}{This is the reweighting factor which has to be given to group B
#'  such that the distribution of their characteristics will be the same as that
#'   of group A. This is computed, in each \code{strata}, as \code{f_A / f_B}.}
#' \item{rw_AB}{This is the reweighting factor which has to be given to group A such that the distribution of their characteristics will be the same as that of group B. This is computed, in each \code{strata}, as \code{f_B / f_A}.}
#' \item{f_A}{estimated frequencies of \code{strata} for group A (in the common support).}
#' \item{f_B}{estimated frequencies of \code{strata} for group B (in the common support).}
#'
#' @examples
#' data(invented_wages)
#' c00 <- common_support_strata2(invented_wages, treatment = "gender",
#'                        variables = c("sector", "education"),
#'                        y = "wage", weights = "sample_weights")
#'
#' f00 <- fhat_strata2(c00)
#' reweight_strata2(f00)
#'
#' @export
reweight_strata2 <- function(.fhat_strata){
  # dots_s <- list(
  #   lazyeval::interp(~var[1] / var[2], var = as.name("fhat")),
  #   lazyeval::interp(~var[2] / var[1], var = as.name("fhat")),
  #   lazyeval::interp(~var[1], var = as.name("fhat")),
  #   lazyeval::interp(~var[2], var = as.name("fhat"))
  # )

  strata <- "strata"
  common_support <- "common_support"
  #   strata <- NULL # to avoid a NOTE from R CMD check (...)
  #   fhat <- NULL   # to avoid a NOTE from R CMD check (...)
  rw_BA <- "rw_BA"; rw_AB <- "rw_AB"; f_A <- "f_A"; f_B <- "f_B"
  fhat <- rlang::sym("fhat")
  fhat <- rlang::enquo(fhat)

  .fhat_strata %>%
    filter2_(.dots = common_support) %>%
    gby_(strata) %>%
    # summarise2_(
    #   .dots = stats::setNames(
    #     dots_s,
    #     c("rw_BA", "rw_AB", "f_A", "f_B")
    #   )
    # )
    dplyr::summarise(
      !! rw_BA := `[`(!! fhat, 1) / `[`(!! fhat, 2),   # fhat[1] / fhat[2]
      !! rw_AB := `[`(!! fhat, 2) / `[`(!! fhat, 1),   # fhat[2] / fhat[1]
      !! f_A := `[`(!! fhat, 1),
      !! f_B := `[`(!! fhat, 2)
    )
}


# #' @rdname reweight_strata
# #' @export

