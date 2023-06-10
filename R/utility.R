#' Compute rate of stationary values
#'
#' Computes the rate of stationary values from one completed data set to another
#' where the non-missing data is included in the denominator of the calculation,
#' and - by definition - does not contribute to the numerator.
#'
#' @inheritParams measure_correlation
#' @return numeric; the total proportion of stationary values across the two
#'   completed data sets.
#'
#' @export
#' @md
stationary_rate <- function(X, Y, X_init, indicator)

    if (length(X) > 0) {
        sum(mapply(function(x, y, indicator) sum(x == y) + sum(!indicator),
                   X, Y, indicator)) /
            sum(sapply(X_init, length))
    } else # no missing data
        1

#' @importFrom rlang expr_deparse
msg_if_not <- function(condition, msg=NULL)
    if (!condition) {
        if (is.null(msg)) rlang::expr_deparse(substitute(condition)) else msg
    }

#' Random samples from the first natural numbers
#'
#' Generate a matrix in which each row is a random sample from the set of
#' the first \code{n} natural numbers.
#'
#' This is the author's best effort at efficiently generating a large number of
#' samples of fixed length (\code{k}) from the set of the first \code{n} natural
#' numbers without replacement. This implementation samples from the set of all
#' combinations when \code{n}, \code{k} and \code{size} are favourable,
#' otherwise it uses a straightforward call to \code{\link{base}{sapply}} and
#' \code{\link{base}{sample.int}}.
#'
#' When sampling with replacement, the implementation is a trivial call to
#' \code{link{sample.int}} and a matrix constructor.
#'
#' @param n integer;
#'     defines set to sample from as \eqn{\{1, \ldots, n\}}.
#' @param k integer;
#'     number of items to draw from the set \eqn{\{1, \ldots, n\}} in each
#'     sample (set), also the number of columns in output.
#' @param size integer;
#'     number of (sample) sets in total, also the number of rows in output.
#' @param replace boolean;
#'     indicate whether to sample with replacement (within the set).
#' @return matrix;
#'     a \code{size} by \code{k} matrix with a random sample from
#'     \eqn{\{1, \ldots, n\}} in each row.
#'
#' @seealso \code{\link{utils}{combn}} \code{\link{base}{sample.int}}
#'
#' @importFrom utils combn
#' @keywords internal
samples_as_matrix <- function(n, k=n, size=0, replace=F) {

    stopifnot(n >= 1 && k >= 1)
    stopifnot(k <= n)
    stopifnot(round(k) == k & round(n) == n)

    if (!replace && round(k) > 1) {
        if (size >= 2 * choose(n, k)) {
          # subset all combinations
            t(utils::combn(x=n, m=k))[
                sample.int(choose(n, k), replace=T, size=size),,drop=F
            ]
        } else {
            # loop is probably faster
            matrix(sapply(rep_len(n, size), sample.int, size=k, replace=F),
                   byrow=T,
                   nrow=size)
        }
    } else {
        matrix(sample.int(n, replace=T, size=size * k), nrow=size)
    }

}

