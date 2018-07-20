#' Compute rate of stationary values
#'
#' Computes the rate of stationary values from one completed data set to another
#' where the non-missing data is included in the denominator of the calculation,
#' and - by definition - does not contribute to the numerator.
#'
#' @inheritParams measure_correlation
#' @return numeric; the total proportion of stationary values across the two
#'             completed data sets.
#'
#' @export
stationary_rate <- function(X, Y, X_init, indicator)

    if (length(X) > 0) { 
        sum(mapply(function(x, y, indicator)
                       sum(x == y) + sum(!indicator),
                   X,
                   Y,
                   indicator)) /
            sum(sapply(X_init, length))
    } else # no missing data
        1

