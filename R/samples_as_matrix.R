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
#' @keywords internal
samples_as_matrix <- function(n, k=n, size=0, replace=F)

    if (!replace) {
        if (size >= 2 * choose(n, k)) {
            # subset all combinations
            t(combn(x=n, m=k))[
                sample.int(choose(n, k), replace=T, size=size),,drop=F
            ]
        } else {
            # loop is probably faster
            t(sapply(rep_len(n, size), sample.int, size=k, replace=F))
        }
    } else {
        matrix(sample.int(n, replace=T, size=size * k), nrow=size)
    }

