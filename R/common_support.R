


#' Establishes the Common Support between two groups of individuals, based on the (empirical) joint distribution of some characteristics (variables).
#'
#' @param data data frame
#' @param treatment column name of the binary variable
#' @param variables character vector of the variables' names, for which the common support has to be established
#'
#' @return a data frame with all the combinations of the modalities of the variables considered. In addition two columns: \code{common_support} (logical, TRUE if in the common support), \code{strata} (identifier of a unique combination of the modalities of the variables)
#'
#' @seealso See \code{\link{common_support_strata2}}, which uses this function and then joins with the original data.
#'
#' @examples
#' data(invented_wages)
#' common_support2(invented_wages, treatment = "gender",
#'                 variables = c("sector", "education"))
#'
#' @export
common_support2 <- function(data, treatment, variables){
  stopifnot(check_char_length_1(treatment))
  stopifnot(check_char_not_0(variables))
  stopifnot(is.data.frame(data))
  stopifnot(length(unique(data[[treatment]])) == 2)

  sel_vars <- c(treatment, variables)
  joint_distr <- data %>%
    dplyr::group_by_(.dots = c(treatment, variables)) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::ungroup() %>%
    tidyr::unite_(col = "strata",
                  from = variables, remove = FALSE) %>%
    tidyr::spread_(key = treatment, value = "n")

#  n_diff <- NULL  # to avoid a NOTE from R CMD check (...)
  k <- ncol(joint_distr)
  k <- c(k-1, k)
  cols <- colnames(joint_distr)[k]
  col1 <- cols[1]; col2 <- cols[2]
  dots <- lazyeval::interp(~ x - y, x = as.name(col1), y = as.name(col2))
  joint_distr <- joint_distr %>%
    mutate_(.dots = stats::setNames(list(dots), c("n_diff"))) %>%
    mutate_(common_support = ~ifelse(is.na(n_diff), FALSE, TRUE))

  k <- check_NA_(joint_distr, variables = variables)
  joint_distr[k, "common_support"] <- FALSE

  attributes(joint_distr)[["treatment"]] <- treatment
  attributes(joint_distr)[["variables"]] <- variables
  joint_distr
}


common_support3 <- function(data, treatment, variables){
  # Check if some variables are of type double
  doubles_ <- check_doubles(data %>% dplyr::select_(.dots = variables))
  if(length(doubles_) > 0) cat(paste0("One or more of the variables are of type double:\n", doubles_, "\nMaybe it is better to create discrete versions of these variables\n"))

  common_support2(data = data,
                  treatment = treatment,
                  variables = variables)
}


