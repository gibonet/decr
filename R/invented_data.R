


#' Invented dataset with wages of men and women.
#'
#' This dataset has been completely invented, in order to do some examples
#' with the decr package.
#'
#' Every row of the dataset consists in a fake/invented individual worker.
#' For every individual there is his/her gender, the economic sector in which
#' he/she works, his/her level of education and his/her wage. Furthermore
#' there is a column with the sampling weights.
#'
#'
#' @format A data frame with 1000 rows and 5 variables:
#' \describe{
#'   \item{\code{gender}}{gender of the worker (\code{men} or \code{women})}
#'   \item{\code{sector}}{economic sector where the worker is employed (\code{secondary} or \code{tertiary})}
#'   \item{\code{education}}{educational level of the worker (\code{I}, \code{II} or \code{III})}
#'   \item{\code{wage}}{monthly wage of the worker (in an invented currency)}
#'   \item{\code{sample_weights}}{sampling weights}
#' }
"invented_wages"


