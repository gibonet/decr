
# dplyr 'wrappers'

# group_by_(.data, .dots = character)
gby_ <- function(.data, .variables){
  .data %>%
    dplyr::group_by_(.dots = .variables)
}
# gby_(mtcars, c("vs", "am"))
# gby_(mtcars, c("vs", "am", "mpg")) %>%
#   summarise(wgroup = sum(cyl)) %>%
#   gby_(c("vs", "am")) %>%
#   mutate(wcum = cumsum(wgroup))
# common_support2, fhat_strata2, reweight_strata2, nopodec_mean, nopodec_Fhat, 
# nopodec, nopodec_stat, dec_median, dec_quantile, margin_quantile


# summarise_(.data, ...)
summarise2_ <- function(.data, ...){
  dplyr::summarise_(.data, ...)
}
# fhat_strata2, reweight_strata2, nopodec_mean, nopodec_Fhat, nopodec, 
# nopodec_stat, dec_median, dec_quantile, margin_quantile, margin_mean


# select_(.data, .dots = character)
select2_ <- function(.data, .variables){
  dplyr::select_(.data, .dots = .variables)
}
# common_support_strata2


# arrange_(.data, .dots = character)
arrange2_ <- function(.data, .variables){
  dplyr::arrange_(.data, .dots = .variables)
}
# fhat_strata2


# filter_(.data, .dots = character_expr)
filter2_ <- function(.data, .dots){
  dplyr::filter_(.data, .dots = .dots)
}
# reweight_strata2, nopodec, nopodec_stat, dec_


mutate2_ <- function(.data, ..., .dots){
  dplyr::mutate_(.data, ..., .dots = .dots)
}
# common_support2, reweight_strata_all2, boot_nopodec_mean, boot_nopodec_quantile, nopodec