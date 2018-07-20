#' Calculate entropy of categorical data
#'
#' Calculate entropy of categorical data using tally of observations in each
#' category.
#'
#' @param x ordered;
#'            observed categorical data.
#' @param freq numeric;
#'            count of observations in each group.
#' @return numeric;
#'             scalar value of entropy.
#'
#' @section To-do:
#' \itemize{
#'   \item document \code{entropy}.
#'   \item write tests for \code{entropy}.
#' }
#'
#' @keywords internal
entropy <- function(x, freq=table(x))
    -sum(freq * log2(freq / sum(freq)), na.rm=T) / sum(freq)

