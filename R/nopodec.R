

#' Decomposition of the observed difference between averages of y of group A and B in 4 components, as in Nopo (2008).
#'
#' The decomposition of the difference is like that developed by Nopo (2008), especially
#' regarding the two components out of support. The decomposition in the common support
#' is instead done with the reweighting approach proposed by Di Nardo, Fortin and Lemieux (1996).
#'
#' @param .nopodec_ output of \code{\link{nopodec_mean}}.
#' @param counterfactual "AB" or "BA". "AB" means that we want to estimate the counterfactual (wage) of group A, as if their characteristics were distributed as in group B. "BA" is the opposite (characteristics of group B are balanced to those of group A).
#'
#' @return a list with five components:
#'
#' \itemize{
#'   \item \code{delta_total}: total observed difference between average (wages)
#'    of group A and B;
#'   \item \code{delta_A}: part of the observed difference due to the fact that
#'   there are indidivuals of group A which are not comparable, for features,
#'    to individuals of group B;
#'   \item \code{delta_X} part explained by the fact that the two groups have a
#'    different distribution of characteristics;
#'   \item \code{delta_S} part not justified by the different distributions of the
#'    characteristics of the two groups, and potentially due to a difference
#'     in the remuneration structures between the two groups;
#'   \item \code{delta_B} part of the difference due to the fact that there are
#'    individuals of group B with characteristics that none of the group A
#'     has.
#' }
#'
#' @references
#' Nopo, H. 2008. Matching as a Tool to Decompose Wage Gaps. \emph{Review of Economics and Statistics}, 90(2): 290-299.
#'
#' DiNardo J., N. M. Fortin and T. Lemieux. 1996. Labor Market Institutions and the Distribution of Wages, 1973-1992: A Semiparametric Approach. \emph{Econometrica} 64 (5): 1001-44
#'
#'
#' @examples
#' data(invented_wages)
#'
#' # Common support and computation of counterfactual weights
#' r00 <- reweight_strata_all2(data = invented_wages,
#'                             treatment = "gender",
#'                             variables = c("sector", "education"),
#'                             y = "wage",
#'                             weights = "sample_weights")
#'
#' # Computation of the elements necessary to the decomposition
#' n00 <- nopodec_mean(r00)
#'
#' # Nopo decomposition
#' nopodec(n00, counterfactual = "AB")
#'
#' @export
nopodec <- function(.nopodec_, counterfactual = c("AB", "BA")){
  stopifnot(counterfactual[1] %in% c("AB", "BA"))

  treatment <- colnames(.nopodec_)[1]
  if(is.factor(.nopodec_[[treatment]])){
    groups_ <- levels(.nopodec_[[treatment]])
  }else{
    groups_ <- unique(.nopodec_[[treatment]])
  }

  sel_A <- lazyeval::interp(~x == y, x = as.name(treatment), y = groups_[1])
  sel_B <- lazyeval::interp(~x == y, x = as.name(treatment), y = groups_[2])

  perc_ <- lazyeval::interp(~x / sum(x), x = as.name("Nhat"))

  common_support <- "common_support"
  sel_common_support <- lazyeval::interp(~!x, x = as.name(common_support))

  phat_AB_out <- .nopodec_ %>%
    gby_(treatment) %>%
    dplyr::mutate_(.dots = stats::setNames(list(perc_), c("perc"))) %>%
    filter2_(.dots = sel_common_support)

  phat_A_out <- (phat_AB_out %>% filter2_(sel_A))$perc
  phat_B_out <- (phat_AB_out %>% filter2_(sel_B))$perc

  ybar_A_out <- (.nopodec_ %>% filter2_(sel_A) %>% dplyr::filter(!common_support))$ybar
  ybar_A_in <- (.nopodec_ %>% filter2_(sel_A) %>% dplyr::filter(common_support))$ybar
  ybar_B_out <- (.nopodec_ %>% filter2_(sel_B) %>% dplyr::filter(!common_support))$ybar
  ybar_B_in <- (.nopodec_ %>% filter2_(sel_B) %>% dplyr::filter(common_support))$ybar

  # I due controfattuali (per una scomposizione ne serve uno dei due, ma la scomposizione
  # può essere fatta con entrambi: X_A'Beta_B e X_B'Beta_A)

  # Salario del gruppo A come se avesse le stesse caratteristiche del gruppo B (X_B'Beta_A)
  ybar_AB_C <- (.nopodec_ %>% filter2_(sel_A) %>% dplyr::filter(common_support))$ybar_C_A

  # Salario del gruppo B come se avesse le stesse caratteristiche del gruppo A (X_A'Beta_B)
  ybar_BA_C <- (.nopodec_ %>% filter2_(sel_B) %>% dplyr::filter(common_support))$ybar_C_B


  ################################################
  # Prova (per il caso no common support)
  if(check_numeric_0(ybar_A_in)) ybar_A_in <- 0
  if(check_numeric_0(ybar_B_in)) ybar_B_in <- 0
  if(check_numeric_0(ybar_AB_C)) ybar_AB_C <- 0
  if(check_numeric_0(ybar_BA_C)) ybar_BA_C <- 0
  ################################################

  # Le due componenti della scomposizione che si trovano fuori dal common support
  # non cambiano in funzione di quale controfattuale si usa (AB o BA)
  # N.B.: per "AB" si intende il controfattuale A come se le sue caratteristiche
  # fossero distribuite come quelle del gruppo B (Xbar_B'Beta_A in termini BO)
  delta_A <- phat_A_out * (ybar_A_out - ybar_A_in)
  delta_B <- phat_B_out * (ybar_B_in - ybar_B_out)

  counterfactual <- match.arg(counterfactual)
  # Prima scomposizione: con ybar_AB_C
  if(counterfactual == "AB"){
    delta_X <- ybar_A_in - ybar_AB_C
    delta_S <- ybar_AB_C - ybar_B_in
  }
  if(counterfactual == "BA"){
    delta_S <- ybar_A_in - ybar_BA_C
    delta_X <- ybar_BA_C - ybar_B_in
  }

  if(check_numeric_0(delta_A)) delta_A <- 0
  if(check_numeric_0(delta_B)) delta_B <- 0

  delta_tot <- delta_A + delta_X + delta_S + delta_B

  list(delta_tot = delta_tot, delta_A = delta_A, delta_X = delta_X,
       delta_S = delta_S, delta_B = delta_B)
}







nopodec_stat <- function(.nopodec_, counterfactual = c("AB", "BA"), stat = c("Fhat", "ybar")){
  stopifnot(counterfactual[1] %in% c("AB", "BA"))

  treatment <- colnames(.nopodec_)[1]
  if(is.factor(.nopodec_[[treatment]])){
    groups_ <- levels(.nopodec_[[treatment]])
  }else{
    groups_ <- unique(.nopodec_[[treatment]])
  }

  sel_A <- lazyeval::interp(~x == y, x = as.name(treatment), y = groups_[1])
  sel_B <- lazyeval::interp(~x == y, x = as.name(treatment), y = groups_[2])

  perc_ <- lazyeval::interp(~x / sum(x), x = as.name("Nhat"))

  common_support <- "common_support"
  sel_common_support <- lazyeval::interp(~!x, x = as.name(common_support))

  phat_AB_out <- .nopodec_ %>%
    gby_(treatment) %>%
    dplyr::mutate_(.dots = stats::setNames(list(perc_), c("perc"))) %>%
    filter2_(.dots = sel_common_support)

  phat_A_out <- (phat_AB_out %>% filter2_(sel_A))$perc
  phat_B_out <- (phat_AB_out %>% filter2_(sel_B))$perc

  ybar_A_out <- (.nopodec_ %>% filter2_(sel_A) %>% dplyr::filter(!common_support))[[stat[1]]]
  ybar_A_in <- (.nopodec_ %>% filter2_(sel_A) %>% dplyr::filter(common_support))[[stat[1]]]
  ybar_B_out <- (.nopodec_ %>% filter2_(sel_B) %>% dplyr::filter(!common_support))[[stat[1]]]
  ybar_B_in <- (.nopodec_ %>% filter2_(sel_B) %>% dplyr::filter(common_support))[[stat[1]]]

  # I due controfattuali (per una scomposizione ne serve uno dei due, ma la scomposizione
  # può essere fatta con entrambi: X_A'Beta_B e X_B'Beta_A)

  stat_C_A <- paste0(stat[1], "_C_A")
  stat_C_B <- paste0(stat[1], "_C_B")
  # Salario del gruppo A come se avesse le stesse caratteristiche del gruppo B (X_B'Beta_A)
  ybar_AB_C <- (.nopodec_ %>% filter2_(sel_A) %>% dplyr::filter(common_support))[[stat_C_A]]

  # Salario del gruppo B come se avesse le stesse caratteristiche del gruppo A (X_A'Beta_B)
  ybar_BA_C <- (.nopodec_ %>% filter2_(sel_B) %>% dplyr::filter(common_support))[[stat_C_B]]


  ################################################
  # Prova (per il caso no common support)
  if(check_numeric_0(ybar_A_in)) ybar_A_in <- 0
  if(check_numeric_0(ybar_B_in)) ybar_B_in <- 0
  if(check_numeric_0(ybar_AB_C)) ybar_AB_C <- 0
  if(check_numeric_0(ybar_BA_C)) ybar_BA_C <- 0
  ################################################

  # Le due componenti della scomposizione che si trovano fuori dal common support
  # non cambiano in funzione di quale controfattuale si usa (AB o BA)
  # N.B.: per "AB" si intende il controfattuale A come se le sue caratteristiche
  # fossero distribuite come quelle del gruppo B (Xbar_B'Beta_A in termini BO)
  delta_A <- phat_A_out * (ybar_A_out - ybar_A_in)
  delta_B <- phat_B_out * (ybar_B_in - ybar_B_out)

  counterfactual <- match.arg(counterfactual)
  # Prima scomposizione: con ybar_AB_C
  if(counterfactual == "AB"){
    delta_X <- ybar_A_in - ybar_AB_C
    delta_S <- ybar_AB_C - ybar_B_in
  }
  if(counterfactual == "BA"){
    delta_S <- ybar_A_in - ybar_BA_C
    delta_X <- ybar_BA_C - ybar_B_in
  }

  if(check_numeric_0(delta_A)) delta_A <- 0
  if(check_numeric_0(delta_B)) delta_B <- 0

  delta_tot <- delta_A + delta_X + delta_S + delta_B

  list(delta_tot = delta_tot, delta_A = delta_A, delta_X = delta_X,
       delta_S = delta_S, delta_B = delta_B)
}
