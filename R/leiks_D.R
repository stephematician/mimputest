#' Calculate Leik's D of ordered data 
#'
#' Calculate Leiks' D of ordered data using tally of observations in each
#' category (provided in order)
#'
#' Calculates the dispersion via the Leik's D statistic for ordinal data using
#' a tally of observations in each category and the following formula; \deqn{
#'    D = \frac{2}{n - 1} \left(
#'            \sum_{j : F_j < 0.5} F_j + \sum_{j : F_j \geq 0.5} 1 - F_j
#'        \right)
#' }
#' where \eqn{n} is the number of categories, and \eqn{F_k} is the cumulative
#' distribution for category indexed by \eqn{k}, except in the case of one
#' category, where the value 0 is returned.
#'
#' @param x ordered;
#'            ordered data or data that can be coerced into ordered via
#'            \code{\link{as.factor}} and \code{\link{factor}} with 
#'            \code{ordered=T}.
#' @param f_x;
#'            data as a factor (or ordered factor).
#' @param freq numeric;
#'            count of observations in each category in order.
#' @param cdf numeric;
#'            cumulative proportion of observations in each category in order.
#' @return numeric;
#'             scalar value of Leik's D
#'
#' @keywords internal
leiks_D <- function(x) {

    x <- as.factor(x)
    freq <- table(factor(x, levels=levels(x), ordered=T))
    cumfreq <- cumsum(freq)
    n <- sum(freq)
    cumfreq <- cumfreq[-length(cumfreq)]

    2 * sum(cumfreq + (n - 2 * cumfreq) * as.numeric(cumfreq >= (n / 2))) /
        (max(length(cumfreq), 1) * n)

}

