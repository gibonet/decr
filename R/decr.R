
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
#' @import rlang
#' @import tidyr
#' @import dplyr
#' @importFrom utils combn
#' @importFrom stats na.omit setNames weighted.mean quantile
#' @importFrom grDevices nclass.Sturges nclass.scott nclass.FD
#' @importFrom boot boot boot.ci
NULL

# Tolto
# #' @import lazyeval
# Tolto anche lazyeval dal file DESCRIPTION (lazyeval (>= 0.1.10))



#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


# #' @import dplyr lazyeval
# #' @importFrom tidyr expand expand_ complete_ unite_
# #' @importFrom dplyr filter group_by left_join mutate n select summarise ungroup as_data_frame do sample_n do_
