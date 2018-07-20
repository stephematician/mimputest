#' Evaluate stop condition of imputation procedure
#'
#' Trivial evaluation of two measures of the inter-iteration relationship
#' between completed data sets. Evaluates to \code{T} if all of the measurements
#' at the current iteration are comparable to and (strictly) less than the
#' measurements at the previous iteration, otherwise returns \code{F}.
#'
#' @param previous_measure numeric;
#'            values of a measure of the relationship between the second most
#'            recent completed data set and that of iteration prior to it.
#' @param measure numeric;
#'            values of a measure of the relationship between the most recent
#'            completed data set and that of the iteration prior to it.
#' @return logical;
#'             \code{T} if all values of \code{previous_measure} are greater
#'             than or equal to corresponding value of \code{measure}.
#'
#' @seealso \code{\link{measure_correlation}}
#'          \code{\link{measure_stekhoven_2012}}
#'
#' @export
stop_condition <- function(previous_measure, measure) {

    if (!all(names(previous_measure) %in% names(measure))) {
        stop('previous measure and measure do not have matching names.')
        previous_measure <- previous_measure[names(measure)]
    }

    identical(unname(previous_measure >= measure), rep(T, length(measure)))

}

