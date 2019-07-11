#' Calculate Leik's D of ordered data 
#'
#' Calculate Leiks' D of ordered data using tally of observations in each
#' category (provided in order)
#'
#' @param x ordered;
#'            observed ordered data or data that can be coerced into ordered
#'            via as.ordered.
#' @param freq numeric;
#'            count of observations in each category in order.
#' @param cdf numeric;
#'            cumulative proportion of observations in each category in order.
#' @return numeric;
#'             scalar value of Leik's D
#'
#' @section To-do:
#' \itemize{
#'   \item document \code{leiks_D}.
#'   \item write tests for \code{leiks_D}.
#' }
#'
#' @keywords internal
leiks_D <- function(x, freq=table(as.ordered(x)), cdf=cumsum(freq) / sum(freq))
    2 * sum(cdf + (1 - 2 * cdf) * as.numeric(cdf >= 0.5)) /
        (length(freq) - 1)

