
# Checks if there is at least one missing value, for the columns 'variables',
# in every row of the data frame 'data'.
# Returns a logical vector of length equal to the number of rows of data,
# with values TRUE if there is at least one missing value (NA) in the
# corresponding row of the data, and FALSE if there are no missing values.
check_NA_ <- function(data, variables){
  k <- data %>% select2_(variables) %>% lapply(is.na)
  k <- Reduce("|", k)
  k
}
# d <- data.frame(x = c("1", "2", NA), y = c(NA, "1", "2"))
# decr:::check_NA_(d, variables = "x")
# decr:::check_NA_(d, variables = "y")
# decr:::check_NA_(d, variables = c("x", "y"))


# cut_c: cut custom
auto_breaks <- function(x, nclass = c("Sturges", "scott", "FD")){
  nclass <- match.arg(nclass)

  breaks_ <- switch(nclass, Sturges = grDevices::nclass.Sturges(x),
                    scott = grDevices::nclass.scott(x),
                    FD = grDevices::nclass.FD(x))
  # breaks_: an integer with the 'desired' number of breaks
  breaks_
}

quantile_breaks <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE, ...){
  # probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)
  # probs = seq(0, 1, by = 0.1)
  breaks_ <- stats::quantile(x, probs = probs, na.rm = na.rm, ...)
  # breaks_: a numeric vector with breaks
  breaks_
}

cut_c <- function(x, breaks = auto_breaks(x), include.lowest = TRUE, right = FALSE){
  cut(x, breaks = breaks, include.lowest = include.lowest, right = right)
}
# cut_c(rnorm(100))
# cut_c(rnorm(100), include.lowest = FALSE, right = TRUE)
# cut_c(rnorm(100), breaks = quantile_breaks(LL$age))
# cut_c(rnorm(100), breaks = quantile_breaks(LL$age, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)))



create_classes <- function(data, variables = colnames(data), ...){
  tmp <- data %>% select2_(variables)

  col_t <- sapply(tmp, typeof)

  doubles <- names(col_t)[col_t == "double"]

  for(i in doubles) tmp[[i]] <- cut_c(tmp[[i]], ...)
  tmp
}
# data(LL)
# (create_classes(LL, variables = c("age")))
# tmp <- (create_classes(LL))


# Verifica se una o più colonne sono numeriche di tipo double
# (ritorna un vettore character con i nomi delle colonne di tipo double,
# se esistono. Se non ce ne sono, ritorna un vettore character di lunghezza zero)
check_doubles <- function(data){
  col_t <- sapply(data, typeof)

  doubles <- names(col_t)[col_t == "double"]
  doubles
}
# data(LL)
# check_doubles(LL)
# check_doubles(LL %>% select(married))




# Da un vettore character con il nome di variabili, crea una formula
# con tutte le interazioni tra le variabili (~X1 * X2 * X3)
full_formula <- function(treatment, variables){
  s1 <- paste0(variables, collapse = " * ")
  s2 <- paste(treatment, "~", s1)
  s2 <- stats::as.formula(s2)
  s2
}
# data(LL)
# vars <- c("age", "education", "married")
# s <- full_formula(treatment = "treated", vars)
# s
# str(s)

# Stima di un pscore
pscore_ <- function(data, treatment, variables){
  s <- full_formula(treatment = treatment, variables = variables)

  plogit <- stats::glm(formula = s, data = data, family = stats::binomial(link = "logit"))
  pscore <- plogit$fitted.values
  pscore
}
# p <- pscore_(data = LL, treatment = "treated", variables = vars)
# str(p)
# names(p)


plogis_ <- function(x){
  1 / (1 + exp(-(x)))
}
# plogis_(p)
#
# plogis_(p) - plogis(p)




#' Estimates empirical weighted quantile
#'
#' @param x A numeric vector
#' @param weights A vector of (positive) sample weights
#' @param probs a numeric vector with the desired quantile levels (default 0.5, the median)
#' @return The weighted quantile (a numeric vector)
#' @references Ferrez, J., Graf, M. (2007). Enquête suisse sur la structure des salaires. Programmes R pour l'intervalle de confiance de la médiane. (Rapport de
#' méthodes No. 338-0045) . Neuchâtel: Office fédéral de la statistique.
#'
#' @examples
#' wq(x = rnorm(100), weights = runif(100))
#' @export
wq <- function(x, weights, probs = c(0.5)){
  if(missing(weights))
    return(stats::quantile(x, probs = probs, na.rm = TRUE))
  ord <- order(x)
  cum.w <- cumsum(weights[ord])[!is.na(x)]/sum(weights[!is.na(x)])
  tmpS <- data.frame(matrix(rep(NA, 2 * length(probs)), nrow = 2))
  tmpO <- data.frame(matrix(rep(NA, 2 * length(probs)), nrow = 2))
  res <- c(rep(NA, length(probs)))
  for (i in 1:length(probs)) {
    tmpS[i] <- cum.w[cum.w >= probs[i]][1:2]
    tmpO[i] <- ord[cum.w >= probs[i]][1:2]
    res[i] <- (ifelse(abs(tmpS[1, i] - probs[i]) < 1e-10,
                      mean(x[tmpO[, i]]), x[tmpO[1, i]]))
  }
  return(res)
}


# some validity checks of function arguments
check_char_length_1 <- function(x){
  is.character(x) && length(x) == 1
}
# check_char_length_1("treatment")
# check_char_length_1(c("treatment", "bla"))
# check_char_length_1(1)


check_char_not_0 <- function(x){
  is.character(x) && length(x) >= 1
}
# check_char_not_0(character(0))
# check_char_not_0("X1")
# check_char_not_0(c("X1", "X2"))



# check if is numeric(0) (numeric vector of length 0)
check_numeric_0 <- function(x){
  is.numeric(x) && length(x) == 0
}
# check_numeric_0(numeric(0))  # must be TRUE
# check_numeric_0(1)           # must be FALSE

