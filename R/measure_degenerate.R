#' Measure if reached limit of iterations
#'
#' A degenerate 'measure' of convergence, which simply returns whether to
#' continue if the limit of the number of iterations has been reached, or not.
#'
#' @inheritParams perform_missforest
#' @param X named list;
#'            imputed values, in order of appearance by row in original data,
#'            of each variable (named) from one iteration within missForest
#'            procedure.
#' @param Y named list;
#'            imputed values, in order of appearance by row in origina data,
#'            of each variable (named) from the iteration within the missForest
#'            procedure succeeding that used to determine \code{X}.
#' @param env environment;
#'            environment of the calling function 
#             \code{\link{perform_missforest}}
#' @return named numeric;
#'             zero when the limit on number of iterations has been reached,
#'             otherwise the current iteration.
#'
#' @seealso \code{\link{smirf}}
#'
#' @examples
#' \dontrun{
#' # simply pass to smirf
#' smirf(iris, stop.measure=measure_degenerate)
#' }
#' @export
measure_degenerate <- function(X, Y, X_init, indicator, env=parent.frame()) {

    if (eval(expression(loop.limit < 2L), envir=env)) {
        NULL
    } else 
        c(measure=eval(expression(j * (j < loop.limit)), envir=env))

}

