
#' Tools to Perform Decompositions of Differences between two Groups
#'
#' Tools to perform decompositions of differences between two groups
#' (for example wage decompositions). Focus is on the common support
#' between the distribution of the characteristics of the two groups.
#' In the common support, counterfactuals can be estimated in order
#' to perform the decomposition.
#'
#' @docType package
#' @name decr
#'
#' @import lazyeval
#' @importFrom utils combn
#' @importFrom stats na.omit setNames weighted.mean quantile
#' @importFrom tidyr unite_
#' @importFrom dplyr arrange_ filter filter_ group_by group_by_ left_join mutate mutate_ n select select_ summarise summarise_ ungroup as_data_frame do sample_n do_
#' @importFrom grDevices nclass.Sturges nclass.scott nclass.FD
#' @importFrom boot boot boot.ci
NULL


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


# #' @import dplyr lazyeval
# #' @importFrom tidyr expand expand_ complete_ unite_
