#' Calculate Leik's D of ordered data 
#'
#' Calculate Leiks' D of ordered data 
#'
#' Calculates the dispersion via the Leik's D statistic for ordinal data using
#' the following formula; \deqn{
#'    D = \frac{2}{n - 1} \left(%
#'            \sum_{k : F_k < 0.5} F_k + \sum_{k : F_k \geq 0.5} 1 - F_k%
#'        \right),%
#' }{
#'    D = 2 \sum (F_{k} if F_{k} < 0.5 or 1 - F_{k} if F_{k} > 0.5) / (n - 1)
#' }
#' where \eqn{n} is the number of categories, and \eqn{F_k}{F_{k}} is the
#' cumulative distribution for category indexed by \eqn{k}, except in the case
#' of one category, where the value 0 is returned.
#'
#' @param x ordered;
#'            ordered data or data that can be coerced into ordered via
#'            \code{\link{as.factor}} and \code{\link{factor}} with 
#'            \code{ordered=T}.
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

